use crate::spinlock::Spinlock;
use crate::{tracer::*, Severity};
use std::num::{NonZeroU128, NonZeroU64};
use std::time::SystemTime;

// no direct relation to Tracer, but is used by DefaultTracer
pub trait SpanCollection: Send + Sync + 'static {
    type Exportable;

    fn open_span(&mut self,
        trace_id: TraceId,
        span_id: SpanId,
        args: SpanArgs
    ) -> Result<SpanCollectionIndex, ()>;

    fn set_attributes(&mut self, idx: SpanCollectionIndex, attrs: AttributeListRef) -> Result<(), ()>;

    fn set_status(&mut self, idx: SpanCollectionIndex, status: SpanStatus);


    fn add_event(&mut self, idx: SpanCollectionIndex, event: EventArgs) -> Result<(), ()>;

    fn close_span(&mut self, idx: SpanCollectionIndex, closed_at: SystemTime);

    fn drop_span(&mut self,
        idx: SpanCollectionIndex,
        dropped_at: SystemTime,
        export: impl Fn(Self::Exportable),
        _private: PrivateMarker
    );

    fn flush(&mut self, export: impl Fn(Self::Exportable));
}

pub struct EnvHeadSampler {
    default_min_severity: Severity,
    specific_min_severities: Vec<(String, Option<Severity>)>,
}

impl EnvHeadSampler {
    pub fn from_env(env_var: &str) -> Self {
        Self::new(std::env::var(env_var).ok())
    }
    pub fn new(rust_trace_env: Option<String>) -> Self {
        let mut default_min_severity = Severity::Trace;
        let mut specific_min_severities = vec![];

        let rust_trace_env = rust_trace_env.unwrap_or(String::new());

        for entry in rust_trace_env.trim().split(",").map(|e| e.trim()).filter(|e| !e.is_empty()) {
            if let Some(sev) = entry.parse::<Severity>().ok() {
                default_min_severity = sev;
            }
            
            let Some((path, sev_text)) = entry.split_once("=") else {
                eprintln!("[ERROR] tracelite: malformed RUST_TRACE entry: field does not follow format: {entry}");
                continue
            };

            let sev = if sev_text.eq_ignore_ascii_case("off") {
                None
            } else {
                let Ok(sev) = sev_text.parse::<Severity>() else {
                    eprintln!("[ERROR] tracelite: malformed RUST_TRACE entry; invalid severity: {sev_text}");
                    continue
                };
                Some(sev)
            };

            specific_min_severities.push((path.to_owned(), sev));
            specific_min_severities.push((format!("{path}::"), sev));
        }

        // NOTE sorts in reverse by path
        specific_min_severities.sort_by(|(p1, _), (p2, _)| p2.cmp(p1));

        Self{ default_min_severity, specific_min_severities }
    }

    pub fn should_sample(&self, severity: Severity, target: &str) -> bool {
        for (path, min_severity) in &self.specific_min_severities {
            if target == path || (path.ends_with("::") && target.starts_with(path)) {
                // if OFF or less-than min_severity for target
                return *min_severity != None && *min_severity <= Some(severity);
            }
        }
        self.default_min_severity <= severity
    }
}

fn default_instrumentation_error_handler(err: InstrumentationError){
    eprintln!("[ERROR] tracelite: instrumentation error: {err}");
}


pub struct DefaultTracerConfig<C, E> {
    head_sampler: EnvHeadSampler,
    collection: C,
    export_sink: E,
    default_span_kind: Option<SpanKind>,
    on_instrumentation_error: Box<dyn Fn(InstrumentationError) + Send + Sync>
}

impl<C, E> DefaultTracerConfig<C, E>
    where C: SpanCollection, E: Fn(C::Exportable) + Send + Sync + 'static
{
    pub fn new(head_sampler: EnvHeadSampler, collection: C, export_sink: E) -> Self {
        Self{
            head_sampler,
            collection,
            export_sink,
            default_span_kind: None,
            on_instrumentation_error: Box::new(default_instrumentation_error_handler),
        }
    }

    pub fn install(self){
        let tracer = DefaultTracer{
            head_sampler: self.head_sampler,
            spans: Spinlock::new(self.collection),
            export_sink: self.export_sink,
            default_span_kind: self.default_span_kind,
            on_instrumentation_error: self.on_instrumentation_error
        };
        globals::set_tracer(Box::new(tracer));
    }

    pub fn default_span_kind(self, kind: SpanKind) -> Self {
        Self{ default_span_kind: Some(kind), ..self }
    }

    pub fn on_instrumentation_error(self, f: impl Fn(InstrumentationError) + Send + Sync + 'static) -> Self {
        Self{ on_instrumentation_error: Box::new(f), ..self }
    }
}

pub struct DefaultTracer<C, E> {
    head_sampler: EnvHeadSampler,
    // NOTE this gets indexed by SpanContext::collect_idx
    spans: Spinlock<C>,
    export_sink: E,
    default_span_kind: Option<SpanKind>,
    on_instrumentation_error: Box<dyn Fn(InstrumentationError) + Send + Sync>
}

impl<C, E> Drop for DefaultTracer<C, E> {
    fn drop(&mut self) {
        println!("[DEBUG] tracelite: drop default tracer")
    }
}

impl<C, E> Tracer for DefaultTracer<C, E>
    where C: SpanCollection, E: Fn(C::Exportable) + Send + Sync + 'static
{
    fn should_sample(&self, severity: Severity, target: &str) -> bool {
        self.head_sampler.should_sample(severity, target)
    }

    fn open_span(&self, mut args: SpanArgs, _private: PrivateMarker) -> LocalSpanRef {
        // generate ids; extract trace id if there is a parent
        let trace_id = args.parent
            .and_then(|p| p.trace_id())
            .unwrap_or_else(|| TraceId(NonZeroU128::new(fastrand::u128(1..)).unwrap()));
        let span_id = SpanId(NonZeroU64::new(fastrand::u64(1..)).unwrap());

        // populate defaults
        if args.kind.is_none() {
            args.kind = self.default_span_kind;
        } 

        let collect_idx = self.spans.lock().open_span(trace_id, span_id, args).unwrap(); // TODO handle err
        LocalSpanRef{ trace_id, span_id, collect_idx }
    }

    fn set_attributes(&self, span: &LocalSpanRef, attrs: AttributeListRef) {
        self.spans.lock().set_attributes(span.collect_idx, attrs).unwrap(); // TODO handle error 
    }

    fn add_event(&self, span: &LocalSpanRef, event_args: EventArgs) {
        self.spans.lock().add_event(span.collect_idx, event_args).unwrap(); // TODO handle error
    }
    fn set_span_status(&self, span: &LocalSpanRef, status: SpanStatus) {
        self.spans.lock().set_status(span.collect_idx, status);
    }

    fn close_span(&self, span: &LocalSpanRef, closed_at: SystemTime) {
        self.spans.lock().close_span(span.collect_idx, closed_at);
    }
    
    fn drop_span(&self, idx: SpanCollectionIndex, dropped_at: SystemTime, private: PrivateMarker) {
        self.spans.lock().drop_span(idx, dropped_at, &self.export_sink, private);
    }
    fn flush(&self) {
        println!("[DEBUG] tracelite: flush");
        self.spans.lock().flush(&self.export_sink);
    }

    fn instrumentation_error(&self, err: InstrumentationError) {
        (self.on_instrumentation_error)(err)
    }
}