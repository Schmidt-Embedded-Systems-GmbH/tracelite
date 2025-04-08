pub mod otlp_micropb;

use crate::{AttributeList, EventArgs, SpanArgs, SpanCollectionIndex, SpanId, SpanStatus, TraceId};
use std::time::SystemTime;

pub trait SpanCollectionConfig {
    type SpanCollection: SpanCollection;
    fn new(service_name: &str, service_attrs: AttributeList) -> Self;
    fn build(self) -> Self::SpanCollection;
}

// no direct relation to Tracer, but is used by DefaultTracer
pub trait SpanCollection: Send + Sync + 'static {
    type Exportable;

    fn open_span(&mut self,
        trace_id: TraceId,
        span_id: SpanId,
        args: SpanArgs,
        opened_at: u64,
    ) -> Result<SpanCollectionIndex, ()>;

    fn set_attributes(&mut self, idx: SpanCollectionIndex, attrs: AttributeList) -> Result<(), ()>;

    fn set_status(&mut self, idx: SpanCollectionIndex, status: SpanStatus);


    fn add_event(&mut self, idx: SpanCollectionIndex, event: EventArgs, occurs_at: u64) -> Result<(), ()>;

    fn close_span(&mut self, idx: SpanCollectionIndex, closed_at: SystemTime);

    fn drop_span(&mut self,
        idx: SpanCollectionIndex,
        dropped_at: u64,
        export: impl Fn(Self::Exportable),
    );

    fn flush(&mut self, export: impl Fn(Self::Exportable));
}