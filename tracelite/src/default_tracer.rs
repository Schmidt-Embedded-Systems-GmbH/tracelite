use crate::spinlock::Spinlock;
use crate::tracer::*;
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

pub fn default_instrumentation_error_handler(err: InstrumentationError){
    eprintln!("[ERROR] tracelite: instrumentation error: {err}");
}

pub struct DefaultTracerConfig<C, E> {
    collection: C,
    export_sink: E,
    default_span_kind: SpanKind,
    on_instrumentation_error: Box<dyn Fn(InstrumentationError) + Send + Sync>
}

impl<C, E> DefaultTracerConfig<C, E>
    where C: SpanCollection, E: Fn(C::Exportable) + Send + Sync + 'static
{
    pub fn new(collection: C, export_sink: E) -> Self {
        Self{
            collection,
            export_sink,
            default_span_kind: SpanKind::Internal,
            on_instrumentation_error: Box::new(default_instrumentation_error_handler),
        }
    }

    pub fn install(self){
        let tracer = DefaultTracer{
            spans: Spinlock::new(self.collection),
            export_sink: self.export_sink,
            default_span_kind: self.default_span_kind,
            on_instrumentation_error: self.on_instrumentation_error
        };
        globals::set_tracer(Box::new(tracer));
    }

    pub fn default_span_kind(self, kind: SpanKind) -> Self {
        Self{ default_span_kind: kind, ..self }
    }

    pub fn on_instrumentation_error(self, f: impl Fn(InstrumentationError) + Send + Sync + 'static) -> Self {
        Self{ on_instrumentation_error: Box::new(f), ..self }
    }
}

pub struct DefaultTracer<C, E> {
    // NOTE this gets indexed by SpanContext::collect_idx
    spans: Spinlock<C>,
    export_sink: E,
    default_span_kind: SpanKind, // TODO use this
    on_instrumentation_error: Box<dyn Fn(InstrumentationError) + Send + Sync>
}

impl<C, E> Tracer for DefaultTracer<C, E>
    where C: SpanCollection, E: Fn(C::Exportable) + Send + Sync + 'static
{
    fn open_span(&self, args: SpanArgs, _private: PrivateMarker) -> LocalSpanRef {
        // generate ids; extract trace id if there is a parent
        let span_id = SpanId(NonZeroU64::new(fastrand::u64(1..)).unwrap());
        let trace_id = args.parent
            .map(|p| p.trace_id())
            .unwrap_or_else(|| TraceId(NonZeroU128::new(fastrand::u128(1..)).unwrap()));

        // populate defaults
        let args = args.kind(self.default_span_kind);

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
        self.spans.lock().flush(&self.export_sink);
    }

    fn instrumentation_error(&self, err: InstrumentationError) {
        (self.on_instrumentation_error)(err)
    }
    
}