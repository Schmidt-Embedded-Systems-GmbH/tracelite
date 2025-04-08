use crate::clocks::Clock;
use crate::export::ExporterConfig;
use crate::id_generators::IdGenerator;
use crate::sampling::{Sampler, SamplingDecision, SamplingResult, StaticSampler};
use crate::span_collections::{SpanCollection, SpanCollectionConfig};
use crate::spinlock::Spinlock;
use crate::{tracer::*, Severity};

// TODO pass log
pub fn default_instrumentation_error_handler(err: InstrumentationError){
    eprintln!("[ERROR] tracelite: instrumentation error: {err}");
}

pub struct DefaultTracerConfig<C, IG, SS, S, SCC, EC> {
    clock: C,
    id_generator: IG,
    static_sampler: SS,
    sampler: S,
    collection: SCC,
    exporter: EC,
    default_span_kind: Option<SpanKind>,
    on_instrumentation_error: Box<dyn Fn(InstrumentationError) + Send + Sync>
}

impl<C, IG, SS, S, SCC, EC> DefaultTracerConfig<C, IG, SS, S, SCC, EC>
where
    C: Clock,
    IG: IdGenerator,
    SS: StaticSampler,
    S: Sampler,
    SCC: SpanCollectionConfig,
    EC: ExporterConfig<Exportable = <SCC::SpanCollection as SpanCollection>::Exportable>,
{
    pub fn new(clock: C, id_generator: IG, static_sampler: SS, sampler: S, collection: SCC, exporter: EC) -> Self {
        Self{
            clock,
            id_generator,
            static_sampler,
            sampler,
            collection,
            exporter,
            default_span_kind: None,
            on_instrumentation_error: Box::new(default_instrumentation_error_handler),
        }
    }

    pub fn static_sampler<SS2>(self, static_sampler: SS2) -> DefaultTracerConfig<C, IG, SS2, S, SCC, EC> {
        DefaultTracerConfig{
            clock: self.clock,
            id_generator: self.id_generator,
            static_sampler,
            sampler: self.sampler,
            collection: self.collection,
            exporter: self.exporter,
            default_span_kind: self.default_span_kind,
            on_instrumentation_error: self.on_instrumentation_error
        }
    }
    
    pub fn sampler<S2>(self, sampler: S2) -> DefaultTracerConfig<C, IG, SS, S2, SCC, EC> {
        DefaultTracerConfig{
            clock: self.clock,
            id_generator: self.id_generator,
            static_sampler: self.static_sampler,
            sampler,
            collection: self.collection,
            exporter: self.exporter,
            default_span_kind: self.default_span_kind,
            on_instrumentation_error: self.on_instrumentation_error
        }
    }

    pub fn span_collection<SCC2>(self, f: impl FnOnce(SCC) -> SCC2) -> DefaultTracerConfig<C, IG, SS, S, SCC2, EC> {
        DefaultTracerConfig{
            clock: self.clock,
            id_generator: self.id_generator,
            static_sampler: self.static_sampler,
            sampler: self.sampler,
            collection: f(self.collection),
            exporter: self.exporter,
            default_span_kind: self.default_span_kind,
            on_instrumentation_error: self.on_instrumentation_error
        }
    }

    pub fn exporter<EC2>(self, f: impl FnOnce(EC) -> EC2) -> DefaultTracerConfig<C, IG, SS, S, SCC, EC2> {
        DefaultTracerConfig{
            clock: self.clock,
            id_generator: self.id_generator,
            static_sampler: self.static_sampler,
            sampler: self.sampler,
            collection: self.collection,
            exporter: f(self.exporter),
            default_span_kind: self.default_span_kind,
            on_instrumentation_error: self.on_instrumentation_error
        }
    }

    pub fn default_span_kind(self, kind: SpanKind) -> Self {
        Self{ default_span_kind: Some(kind), ..self }
    }

    pub fn on_instrumentation_error(self, f: impl Fn(InstrumentationError) + Send + Sync + 'static) -> Self {
        Self{ on_instrumentation_error: Box::new(f), ..self }
    }

    pub fn install(self){
        let collection = self.collection.build();
        let export_sink = self.exporter.build();

        let tracer = DefaultTracer{
            clock: self.clock,
            id_generator: self.id_generator,
            static_sampler: self.static_sampler,
            sampler: self.sampler,
            spans: Spinlock::new(collection),
            export_sink,
            default_span_kind: self.default_span_kind,
            on_instrumentation_error: self.on_instrumentation_error,
        };
        globals::set_tracer(Box::new(tracer));
    }


}

pub struct DefaultTracer<C, ID, SS, S, SC, X> {
    clock: C,
    id_generator: ID,
    static_sampler: SS,
    sampler: S,
    // NOTE this gets indexed by SpanContext::collect_idx
    spans: Spinlock<SC>,
    export_sink: X,
    default_span_kind: Option<SpanKind>,
    on_instrumentation_error: Box<dyn Fn(InstrumentationError) + Send + Sync>
}

impl<C, ID, SS, S, SC, X> Tracer for DefaultTracer<C, ID, SS, S, SC, X>
where
    C: Clock,
    ID: IdGenerator,
    SS: StaticSampler,
    S: Sampler,
    SC: SpanCollection,
    X: Fn(SC::Exportable) + Send + Sync + 'static,
{
    fn is_enabled(&self, target: Option<&str>, severity: Option<Severity>) -> bool {
        self.static_sampler.is_enabled(target, severity)
    }

    fn start_span(&self, mut args: SpanArgs, _: &mut PrivateMarker) -> Option<(RecordingSpanContext, SamplingResult)> {
        let sampling = self.sampler.should_sample(&args);
        match sampling.decision {
            SamplingDecision::Drop => return None,
            SamplingDecision::RecordOnly => {}, // TODO mark as sampled=false
            SamplingDecision::RecordAndSample => {},
        }

        // set default span kind
        args.kind = args.kind.or(self.default_span_kind);

        // generate ids; extract trace id if there is a parent
        let trace_id = args.parent.as_ref()
            .map(|p| p.trace_id)
            .unwrap_or_else(|| self.id_generator.new_trace_id());
        let span_id = self.id_generator.new_span_id();

        let span_context = SpanContext{ trace_id, span_id, trace_flags: 1 };
        let opened_at = self.clock.now_unix_nano();

        // create Span in SpanCollection
        let Some(collect_idx) = self.spans.lock().open_span(trace_id, span_id, args, opened_at).ok() else {
            // TODO out of memory, what to do now?
            return None
        };

        Some((RecordingSpanContext{ span_context, collect_idx }, sampling))
    }

    fn set_attributes(&self, span: RecordingSpanContext, attrs: AttributeList) {
        let _result = self.spans.lock().set_attributes(span.collect_idx, attrs); // TODO handle err
    }

    fn add_event(&self, span: RecordingSpanContext, args: EventArgs, _: &mut PrivateMarker) {
        let occurs_at = self.clock.now_unix_nano();
        let _result = self.spans.lock().add_event(span.collect_idx, args, occurs_at); // TODO handle err
    }

    fn set_status(&self, span: RecordingSpanContext, status: SpanStatus) {
        let _result = self.spans.lock().set_status(span.collect_idx, status); // TODO handle err
    }

    fn drop_span(&self, span: RecordingSpanContext, _ : &mut PrivateMarker) {
        let dropped_at = self.clock.now_unix_nano();
        self.spans.lock().drop_span(span.collect_idx, dropped_at, &self.export_sink);
    }

    fn flush(&self) {
        #[cfg(feature = "log")]
        log::debug!("flush");
        self.spans.lock().flush(&self.export_sink);
    }

    fn instrumentation_error(&self, err: InstrumentationError) {
        #[cfg(feature = "log")]
        log::error!("instrumentation_error");
        (self.on_instrumentation_error)(err)
    }
}