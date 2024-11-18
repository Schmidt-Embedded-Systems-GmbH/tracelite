#![deny(warnings)]

extern crate tracelite_macro;
#[allow(unused)]
pub use tracelite_macro::{trace_span, debug_span, info_span};

pub use log;

mod spinlock;

mod severity;
pub use severity::*;

mod tracer;
pub use tracer::*;
pub use tracer::globals::*;

mod default_tracer;
pub use default_tracer::*;

pub mod export;

mod otlp_micropb;
pub use otlp_micropb::*;

mod macros;
