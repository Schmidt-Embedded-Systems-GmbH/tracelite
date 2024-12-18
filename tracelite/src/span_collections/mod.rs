mod otlp_micropb;
pub use otlp_micropb::{OtlpMicroPbConfig, OtlpMicroPbSpanCollection};

use crate::{AttributeListRef, EventArgs, SpanArgs, SpanCollectionIndex, SpanId, SpanStatus, TraceId};
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
    );

    fn flush(&mut self, export: impl Fn(Self::Exportable));
}