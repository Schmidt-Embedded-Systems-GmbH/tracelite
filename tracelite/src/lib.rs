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

pub mod sampling;
pub mod span_collections;
pub mod export;

// mod otlp_micropb;
// pub use otlp_micropb::{OtlpMicroPbSpanCollection, OtlpMicroPbConfig};

mod macros;

pub fn install_tracer_micropb_tokio_h2grpc(
    env_var: &str,
    (service_name, service_attributes): (&str, AttributeListRef),
    otlp_endpoint: &str,
    tracer_autoflush_interval: std::time::Duration,
){
    use self::sampling::{AlwaysSampler, EnvStaticSampler};
    use self::span_collections::OtlpMicroPbConfig;

    DefaultTracerConfig::new(
        EnvStaticSampler::from_env(env_var),
        AlwaysSampler,
        OtlpMicroPbConfig::new(service_name, service_attributes)
            .build(),
        export::spawn_tokio_export_task(
            export::H2GrpcExport::new(otlp_endpoint).unwrap(),
            tracer_autoflush_interval,
        )
    ).install();
}

