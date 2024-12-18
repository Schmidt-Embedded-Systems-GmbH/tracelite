use crate::{SpanId, TraceId};
use std::num::{NonZeroU128, NonZeroU64};
use std::sync::atomic;

pub trait IdGenerator: Send + Sync + 'static {
    fn new_trace_id(&self) -> TraceId;
    fn new_span_id(&self) -> SpanId;
}

pub struct FastrandIdGenerator;

impl IdGenerator for FastrandIdGenerator {
    fn new_trace_id(&self) -> TraceId {
        TraceId(NonZeroU128::new(fastrand::u128(1..)).unwrap())
    }

    fn new_span_id(&self) -> SpanId {
        SpanId(NonZeroU64::new(fastrand::u64(1..)).unwrap())
    }
}

#[derive(Default)]
pub struct TestIdGenerator {
    next_trace_id: atomic::AtomicU32,
    next_span_id: atomic::AtomicU32,
}

impl IdGenerator for TestIdGenerator {
    fn new_trace_id(&self) -> TraceId {
        let u = self.next_trace_id.fetch_add(1, atomic::Ordering::SeqCst);
        TraceId(NonZeroU128::new(u as u128 + 1).unwrap())
    }

    fn new_span_id(&self) -> SpanId {
        let u = self.next_span_id.fetch_add(1, atomic::Ordering::SeqCst);
        SpanId(NonZeroU64::new(u as u64 + 1).unwrap())
    }
}