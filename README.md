# tracelite

## Usage

### Add `tracelite` dependency

Add `tracelite` in your `Cargo.toml`:

```toml
[dependencies]
tracelite = "0.1.2"
```

At the moment, there are no features to enable or disable.

### Create spans (no-async rust)

Use `#[info_span]` attribute macro to automatically instrument a function:

```rust
use tracelite::info_span;

#[info_span(yak)]
pub fn shave(yak: usize) -> Result<(), Box<dyn Error + 'static>> {
    /* ... */
    Ok(())
}
```

This is equivalent to using the `new_info_span!` expression macro:

```rust
use tracelite::new_info_span;

pub fn shave(yak: usize) -> Result<(), Box<dyn Error + 'static>> {
    let span = new_info_span!("shave", yak);
    tracelite::in_span(span, || {
        /* ... */
        Ok(())
    })
}
```

Note than creating a span with `new_info_span!` does *not* mean your code enter it.
A created `span` will only be entered inside the  `closure` of `in_span(span, closure)`.

### Create spans (async rust)

The attribute macros will work on async functions, non-async functions, and even [`async_trait`](https://docs.rs/async-trait/latest/async_trait/) functions all the same.

When your code is async, `tracelite` will use the [`InSpan::in_span(span)`](https://docs.rs/tracelite/latest/tracelite/trait.InSpan.html) method.
The `InSpan` trait is automatically implemented for every `std::future::Future`:

```rust
use tracelite::{new_info_span, InSpan};

pub async fn async_shave(yak: usize) -> Result<(), Box<dyn Error + 'static>> {
    let span = new_info_span!("shave", yak);
    async {
        /* ... */
        Ok(())
    }.in_span(span).await;
}
```

Typically, you can always just use the attribute macros. However, **whenever you spawn new tokio tasks,
you will have to call `InSpan::in_span` to properly associate the new task with the parent span**.

```rust

#[info_span(yaks)]
pub fn spawn_shave_task(yaks: usize){
    let span = new_info_span!("shave", yak);

    tokio::spawn(async { 
        for yak in 1..=yaks {
            // NOTE no need for InSpan::in_span here, as the outer async{} block is instrumented
            async_shave(yak).await
        }
    }.in_span(span)); // <---- here, InSpan::in_span is needed
}
```

### Span attributes

In span macros, you can specify a list of span attributes in the `name (:CAPTURE)* (= VALUE)*` format.

|syntax|meaning|
|-|-|
|`attr = value`| capture the attribute value directly (works for primitive types `bool`, `i8`, `i16`, .., and `&str`).
|`attr:% = value`| capture the attribute by it's `std::fmt::Display` representation
|`attr:? = value`| capture the attribute by it's `std::fmt::Debug` representation
|`attr:serde = value`| capture the attribute by it's `serde::Serialize` representation

When you omit the `= VALUE` part, it will use the `attr` variable instead:

```rust
in_new_span!("foo", attr1, attr2:%)

// equivalent to
in_new_span!("foo", attr1 = attr1, attr2:% = attr2)
```

After span creation, you can use the `span_attributes!()` expression macro to add or overwrite span attributes dynamically.

```rust

#[info_span(attr1, attr2:%)]
fn foobar(attr1: &str, attr2: impl std::fmt::Display){
    if attr1 == "give me more!" {
        // overwrite `attr1` attribute
        // add `attr3` attribute
        span_attributes!(attr1 = "got more", attr3 = "more! more!");
    }
}
```

### Span severity

There are 5 different severity levels for spans. The severity is tied to the attribute macro or expression macro that is used:

|severity|attribute macro|expression macro|
|-|-|-
|TRACE|`#[trace_span]`|`in_trace_span!()`|
|DEBUG|`#[debug_span]`|`in_debug_span!()`|
|INFO |`#[info_span]` |`in_info_span!()`|
|WARN |`#[warn_span]` |`in_warn_span!()`|
|ERROR|`#[error_span]`|`in_error_span!()`|


The severity is used for sampling (TODO) and cannot be changed after the span is created.

As a special case, an ERROR span will have an initial [*error* span status](#span-status), which can be overwritten after the span creation:

```rust
#[error_span]
fn some_error_span(foo: bool){
    // if `foo`, then don't highlight this span as an error
    if foo {
        tracelite::set_status(tracelite::SpanStatus::Ok);
    }
}
```

### Span status

A span can have one of 3 possible error statusses:

|span status|meaning|
|-|-
|_unset_|Everything is assumed to be fine
|_error_|An error occurred (span gets highlighted in trace)
|_ok_|An error occurred, but we handled it (span does not get highlighted in trace)

Initially, the span status of a span will be _unset_ (except for ERROR spans). You can change the status of the current span with [`set_status()`](https://docs.rs/tracelite/latest/tracelite/fn.set_status.html) or with the shorthands [`mark_span_as_ok()`](https://docs.rs/tracelite/latest/tracelite/fn.mark_span_as_ok.html)/[`mark_span_as_error()`](https://docs.rs/tracelite/latest/tracelite/fn.mark_span_as_error.html):

```rust
#[error_span]
fn i_am_not_an_error(){
    tracelite::set_status(tracelite::SpanStatus::Ok)
    
    // equivalent to
    tracelite::mark_span_as_ok()
}
#[info_span]
fn i_am_an_error(){
    tracelite::set_status(tracelite::SpanStatus::error("some error message"))

    // equivalent to
    tracelite::mark_span_as_error("some error message")
}
```

### Span events

Use `info_event!()` to add an event to the current span:

```rust
#[info_span]
fn some_span_with_event(arg1: u32, arg2: impl std::fmt::Debug){
    info_event!("i am a event", arg1, arg2:?);
}
```

Event attributes can be specified with the same syntax as [span attributes](#span-attributes).

There are 5 different event severities:

|severity|expression macro
|-|-
|(same as span)|`event!()`|
|TRACE|`trace_event!()`|
|DEBUG|`debug_event!()`|
|INFO |`info_event!()`|
|WARN |`warn_event!()`|
|ERROR|`error_event!()`|

The event severity will be used for sampling (TODO). The event severity **does not raise the severity of its span**. There is no reason to create a INFO event inside a DEBUG span.

As a special case, an ERROR event will set an [*error* span status](#span-status), which can be overwritten afterwards (e.g. if the error has been successfully handled):

```rust
#[info_span]
fn do_work() -> Result<(), dyn Box<std::error::Error>> {
    if let Err(err) = try_something() {
        error_event!("failed_something", err:%); // changes span status to _error_

        if handle_error(err)? {
            tracelite::mark_span_as_ok(); // changes span status to _ok_
        } else {
            // otherwise, span status is still _error_
            return Err(err)
        }
    }

    /* span status is _unset_ or _ok_ */
    continue_work();
}
```

Because tracing an `Err(_)` variant of a `Result` is such a common pattern, you can use the [`RecordException`](https://docs.rs/tracelite/latest/tracelite/trait.RecordException.html) utility trait instead:

```rust
use tracelite::RecordException; // record_exception, record_exception_as_error

#[info_span]
fn do_work() -> Result<(), dyn Box<std::error::Error>> {
    if let Err(err) = try_something().record_exception_as_error("failed_something") {
        /* changes span status is err */

        if handle_error(err).record_exception()? {
            tracelite::mark_span_as_ok(); // changes span status to _ok_
        } else {
            // otherwise, span status is still _error_
            return Err(err)
        }
    }

    /* span status is _unset_ or _ok_ */
    continue_work();
}
```

`RecordException::record_exception` will keep the span status unchanged, while  `RecordException::record_exception_as_error` will set the span status to _error_ with some message. This is useful when you do not want to overwrite a previous error status message.