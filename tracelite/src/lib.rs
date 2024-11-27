#![deny(warnings)]

extern crate tracelite_macro;
#[allow(unused)]
pub use tracelite_macro::{trace_span, debug_span, info_span};

mod spinlock;

mod severity;
pub use severity::{Severity, SeverityParseError};

mod attribute_value;
pub use attribute_value::AttributeValue;

mod tracer;
pub use tracer::*;
pub use tracer::globals::*;

mod default_tracer;
pub use default_tracer::{EnvHeadSampler, DefaultTracer, DefaultTracerConfig};

pub mod export;

mod otlp_micropb;
pub use otlp_micropb::{OtlpMicroPbSpanCollection, OtlpMicroPbConfig};

mod macros;