#![deny(warnings)]

extern crate tracelite_macro;

pub use tracelite_macro::*;

mod spinlock;

mod severity;
pub use severity::{Severity, SeverityParseError};

mod attribute_value;
pub use attribute_value::AttributeValue;

mod tracer;
pub use tracer::*;
pub use tracer::globals::*;

mod default_tracer;
pub use default_tracer::{DefaultTracer, DefaultTracerConfig};

pub mod clocks;
pub mod id_generators;
pub mod sampling;
pub mod span_collections;
pub mod export;

mod macros;

pub fn install_tracer_micropb_tokio_h2grpc(
    env_var: &str,
    (service_name, service_attributes): (&str, AttributeList),
    otlp_endpoint: &str,
    tracer_autoflush_interval: std::time::Duration,
){
    use self::clocks::StdClock;
    use self::id_generators::FastrandIdGenerator;
    use self::sampling::{AlwaysSampler, EnvStaticSampler};
    use self::span_collections::OtlpMicroPbConfig;

    DefaultTracerConfig::new(
        StdClock,
        FastrandIdGenerator,
        EnvStaticSampler::from_env(env_var),
        AlwaysSampler::default(),
        OtlpMicroPbConfig::new(service_name, service_attributes)
            .build(),
        export::spawn_tokio_export_task(
            export::H2GrpcExport::new(otlp_endpoint).unwrap(),
            tracer_autoflush_interval,
        )
    ).install();
}

use self::clocks::TestClock;
use self::sampling::Sampler;
use self::export::TestExport;

pub fn install_tracer_micropb_tokio_test(
    rust_trace_env: &str,
    (service_name, service_attributes): (&str, AttributeList),
    tracer_autoflush_interval: std::time::Duration,
    sampler: impl Sampler,
) -> (TestClock, TestExport) {
    use self::sampling::EnvStaticSampler;
    use self::id_generators::TestIdGenerator;
    use self::span_collections::OtlpMicroPbConfig;

    let test_clock = TestClock::default();
    let test_export = TestExport::default();

    DefaultTracerConfig::new(
        test_clock.clone(),
        TestIdGenerator::default(),
        EnvStaticSampler::new(Some(rust_trace_env.to_owned())),
        sampler,
        OtlpMicroPbConfig::new(service_name, service_attributes)
            .build(),
        export::spawn_tokio_export_task(
            test_export.clone(),
            tracer_autoflush_interval,
        )
    ).install();

    (test_clock, test_export)
}
