mod env_static_sampler;
pub use env_static_sampler::*;

use crate::{Severity, SpanArgs};

pub trait StaticSampler: Send + Sync + 'static {
    fn is_enabled(&self, target: Option<&str>, severity: Option<Severity>) -> bool;
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SamplingDecision {
    Drop,
    RecordOnly,
    RecordAndSample,
}

pub struct SamplingResult {
    pub decision: SamplingDecision,
    pub min_recording_severity: Option<Severity>,
    pub min_sampling_severity: Option<Severity>,
}

pub trait Sampler: Send + Sync + 'static {
    fn should_sample(&self, args: &SpanArgs) -> SamplingResult;
}

#[derive(Default)]
pub struct AlwaysSampler {
    pub min_recording_severity: Option<Severity>,
    pub min_sampling_severity: Option<Severity>,
}

impl Sampler for AlwaysSampler {
    fn should_sample(&self, _: &SpanArgs) -> SamplingResult {
        SamplingResult{
            decision: SamplingDecision::RecordAndSample,
            min_recording_severity: self.min_recording_severity,
            min_sampling_severity: self.min_sampling_severity,
        }
    }
}