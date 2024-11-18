// #![deny(unused)]
#![deny(warnings)]

extern crate tracelite_macro;
#[allow(unused)]
pub use tracelite_macro::{trace_span, debug_span, info_span};

pub use log;

mod spinlock;

mod tracer;
mod default_tracer;
pub use default_tracer::*;

pub mod otlp_micropb;
pub mod export;
pub mod macros;
mod severity;

pub use tracer::*;
pub use tracer::globals::*;
pub use severity::*;

