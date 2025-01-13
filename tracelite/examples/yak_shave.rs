// TODO properly credit
// https://github.com/tokio-rs/tracing

#[macro_use] extern crate tracelite;

use std::{error::Error, io};

use tracelite::{sampling, Severity};


// use tracelite::{debug_event, info_event, warn_event};


// the `#[tracing::instrument]` attribute creates and enters a span
// every time the instrumented function is called. The span is named after
// the function or method. Parameters passed to the function are recorded as fields.
#[info_span( kind(tracelite::SpanKind::Internal), baggage_entry("foo", "bar"), on_start(|_| {}), name(format_args!("shave {yak}")), kind(tracelite::SpanKind::Internal), yak, ?foo=vec![1,2,3])]
// #[info_span(name(format_args!("shave {yak}")), name("foo"))]
pub fn shave(yak: usize) -> Result<(), Box<dyn Error + 'static>> {
    info_event!("hello! I'm gonna shave a yak.", excitement = "yay!");
    if yak == 3 {
        warn_event!("could not locate yak!");
        // note that this is intended to demonstrate `tracing`'s features, not idiomatic
        // error handling! in a library or application, you should consider returning
        // a dedicated `YakError`. libraries like snafu or thiserror make this easy.
        return Err(io::Error::new(io::ErrorKind::Other, "shaving yak failed!").into());
    } else {
        debug_event!("yak shaved successfully");
    }
    Ok(())
}

#[info_span(name("SHAVE_ALL"))]
pub fn shave_all(yaks: usize) -> usize {
    // Constructs a new span named "shaving_yaks" at the TRACE level,
    // and a field whose key is "yaks". This is equivalent to writing:
    //
    // let span = span!(Level::TRACE, "shaving_yaks", yaks = yaks);
    //
    // local variables (`yaks`) can be used as field values
    // without an assignment, similar to struct initializers.
    let span = new_trace_span!("shaving_yaks", yaks);

    tracelite::sync_in_span(span, || {
        info_event!("shaving yaks");

        let mut yaks_shaved = 0;
        for yak in 1..=yaks {
            let res = shave(yak);
            debug_event!("yak", shaved = res.is_ok());

            if let Err(ref error) = res {
                // Like spans, events can also use the field initialization shorthand.
                // In this instance, `yak` is the field being initialized.
                error_event!("failed_to_shave_yak", %error = error.as_ref());
            } else {
                yaks_shaved += 1;
            }
            debug_event!("yaks_shaved", yaks_shaved);
        }

        yaks_shaved
    })
} 

#[info_span(?arg1, ?arg2, %_return,)]
async fn foo(arg1: u32, arg2: u32) -> impl std::fmt::Display {
    arg1 * arg1
}

#[tokio::main]
async fn main(){
    let (test_clock, test_export) = tracelite::install_tracer_micropb_tokio_test(
        "trace",
        ("testing", tracelite::AttributeList(&[])),
        std::time::Duration::from_secs(2),
        sampling::AlwaysSampler{
            min_recording_severity: Some(Severity::Warn),
            min_sampling_severity:  Some(Severity::Warn),
        }
    );

    test_clock.advance(1000);

    let span = new_info_span!("shave_10_yaks");
    tracelite::sync_in_span(span, || {
        let num_shaved = shave_all(10);

        test_clock.advance(2000);

        info_event!("return", num_shaved)
    });
    std::thread::sleep(std::time::Duration::new(20, 0));

    println!("{:?}", test_export.requests.lock().await);
}