#![deny(warnings)]

extern crate tracelite_macro;

pub use tracelite_macro::*;

mod spinlock;

mod severity;
pub use severity::{Severity, SeverityParseError};

mod attributes;
pub use attributes::AttributeValue;

mod tracer;
pub use tracer::*;
pub use tracer::globals::*;

mod record_exception;
pub use record_exception::{RecordException, RecordExceptionDebugFmt};

mod default_tracer;
pub use default_tracer::{DefaultTracer, DefaultTracerConfig};

use self::clocks::StdClock;
use self::export::{h2grpc, ExporterConfig};
use self::id_generators::FastrandIdGenerator;
use self::sampling::AlwaysSampler;
use self::span_collections::{otlp_micropb, SpanCollectionConfig};

pub mod clocks;
pub mod id_generators;
pub mod sampling;
pub mod span_collections;
pub mod export;

mod macros;

pub fn micropb_h2grpc_config(
    otlp_endpoint: &str,
    service_name: &str,
    service_attributes: AttributeList,
    autoflush_interval: std::time::Duration,
) -> DefaultTracerConfig<StdClock, FastrandIdGenerator, AlwaysSampler, AlwaysSampler, otlp_micropb::SpanCollectionConfig, h2grpc::ExporterConfig> {
    let collection = otlp_micropb::SpanCollectionConfig::new(service_name, service_attributes);
    let export = h2grpc::ExporterConfig::new(otlp_endpoint.to_owned(), autoflush_interval);
    DefaultTracerConfig::new(StdClock, FastrandIdGenerator, AlwaysSampler, AlwaysSampler, collection, export)
    // DefaultTracerConfig::new(
    //     StdClock,
    //     FastrandIdGenerator,
    //     AlwaysSampler,
    //     AlwaysSampler,
    //     SpanCollectionConfig::new(service_name, service_attributes)
    //         .build(),
    //     export::spawn_tokio_export_task(
    //         export::H2GrpcExport::new(otlp_endpoint).unwrap(),
    //         autoflush_interval,
    //     ),
    //     default_tracer::disabled_log,
    // )
}

// pub fn install_tracer_micropb_tokio_h2grpc(
//     env_var: &str,
//     (service_name, service_attributes): (&str, AttributeList),
//     otlp_endpoint: &str,
//     tracer_autoflush_interval: std::time::Duration,
// ){
//     use self::clocks::StdClock;
//     use self::id_generators::FastrandIdGenerator;
//     use self::sampling::{AlwaysSampler, EnvSampler};
//     use self::span_collections::OtlpMicroPbConfig;

//     DefaultTracerConfig::new(
//         StdClock,
//         FastrandIdGenerator,
//         EnvSampler::from_env(env_var),
//         AlwaysSampler,
//         OtlpMicroPbConfig::new(service_name, service_attributes)
//             .build(),
//         export::spawn_tokio_export_task(
//             export::H2GrpcExport::new(otlp_endpoint).unwrap(),
//             tracer_autoflush_interval,
//         )
//     ).install();
// }

// use self::clocks::{StdClock, TestClock};
// use self::id_generators::FastrandIdGenerator;
// use self::sampling::{AlwaysSampler, Sampler};
// use self::export::TestExport;
// use self::span_collections::OtlpMicroPbSpanCollection;

// pub fn install_tracer_micropb_tokio_test(
//     rust_trace_env: &str,
//     (service_name, service_attributes): (&str, AttributeList),
//     tracer_autoflush_interval: std::time::Duration,
//     sampler: impl Sampler,
// ) -> (TestClock, TestExport) {
//     use self::sampling::EnvSampler;
//     use self::id_generators::TestIdGenerator;
//     use self::span_collections::OtlpMicroPbConfig;

//     let test_clock = TestClock::default();
//     let test_export = TestExport::default();

//     DefaultTracerConfig::new(
//         test_clock.clone(),
//         TestIdGenerator::default(),
//         EnvSampler::new(Some(rust_trace_env.to_owned())),
//         sampler,
//         OtlpMicroPbConfig::new(service_name, service_attributes)
//             .build(),
//         export::spawn_tokio_export_task(
//             test_export.clone(),
//             tracer_autoflush_interval,
//         )
//     ).install();

//     (test_clock, test_export)
// }
