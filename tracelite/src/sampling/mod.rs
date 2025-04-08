mod env_sampler;
pub use env_sampler::*;

use crate::{Severity, SpanArgs};

pub trait StaticSampler: Send + Sync + 'static {
    fn is_enabled(&self, target: Option<&str>, severity: Option<Severity>) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SamplingDecision {
    Drop,
    RecordOnly,
    RecordAndSample,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynamicTraceDetail {
    pub recording_level: Severity,
    pub sampling_level: Severity,
}

impl From<Severity> for DynamicTraceDetail {
    fn from(sev: Severity) -> Self {
        Self{ recording_level: sev, sampling_level: sev }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SamplingResult {
    pub decision: SamplingDecision,
    pub dyn_trace_detail: Option<DynamicTraceDetail>,
}

impl From<SamplingDecision> for SamplingResult {
    fn from(decision: SamplingDecision) -> Self {
        Self{ decision, dyn_trace_detail: None }
    }
}

pub trait Sampler: Send + Sync + 'static {
    fn should_sample(&self, args: &SpanArgs) -> SamplingResult;
}

// TODO move into dedicated file
pub struct AlwaysSampler;

impl StaticSampler for AlwaysSampler{
    fn is_enabled(&self, _: Option<&str>, _: Option<Severity>) -> bool {
        true
    }
}

impl Sampler for AlwaysSampler {
    fn should_sample(&self, _: &SpanArgs) -> SamplingResult {
        SamplingDecision::RecordAndSample.into()
    }
}