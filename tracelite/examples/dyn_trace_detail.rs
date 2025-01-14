// TODO properly credit
// https://github.com/tokio-rs/tracing

#[macro_use] extern crate tracelite;

use tracelite::{sampling, Severity};
use std::{error::Error, io};


#[tokio::main]
async fn main(){
    let (test_clock, test_export) = tracelite::install_tracer_micropb_tokio_test(
        "trace",
        ("testing", tracelite::AttributeList(&[])),
        std::time::Duration::from_secs(2),
        sampling::AlwaysSampler,
    );

    test_clock.advance(1000);

    let span = new_info_span!("shave_10_yaks");
    tracelite::sync_in_span(span, || {

        test_clock.advance(2000);

    });
    std::thread::sleep(std::time::Duration::new(20, 0));

    println!("{:?}", test_export.requests.lock().await);
}