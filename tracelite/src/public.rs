use crate::internal::{globals::{self, SpanContextDropper}, MaybeStaticStr, SpanArgs, SpanRef, SpanEventArgs};
use tokio::time::Instant;
use std::future::Future;

pub fn current_span() -> Option<SpanRef> {
    globals::current_span()
}

pub async fn in_new_span<'a, const N: usize, T>(
    name: MaybeStaticStr<'a>,
    parent: impl Into<Option<SpanRef>>,
    attributes: impl FnOnce() -> [(MaybeStaticStr<'a>, log::kv::Value<'a>); N],
    log_level: Option<log::Level>,
    fut: impl Future<Output = T>,
) -> T {
    match globals::recorder() {
        Some(tracer) => {
            let args = SpanArgs{ name, parent: parent.into(), opened_at: Instant::now(), init_attributes: &attributes(), log_level };
            let new_span = tracer.open_span(args);
            globals::SPAN_CONTEXT.scope(SpanContextDropper{ ctx: Some(SpanRef::Local(new_span)) }, fut).await
        }
        None => fut.await
    }
}

pub fn sync_in_new_span<'a, const N: usize, T>(
    name: MaybeStaticStr<'a>,
    parent: impl Into<Option<SpanRef>>,
    attributes: impl FnOnce() -> [(MaybeStaticStr<'a>, log::kv::Value<'a>); N],
    log_level: Option<log::Level>,
    closure: impl FnOnce() -> T
) -> T {
    match globals::recorder() {
        Some(tracer) => {
            let args = SpanArgs{ name, parent: parent.into(), opened_at: Instant::now(), init_attributes: &attributes(), log_level };
            let new_span = tracer.open_span(args);
            globals::SPAN_CONTEXT.sync_scope(SpanContextDropper{ ctx: Some(SpanRef::Local(new_span)) }, closure)
        }
        None => closure()
    }
}

pub fn span_event<'a, const N: usize>(
    name: MaybeStaticStr<'a>,
    attributes: impl FnOnce() -> [(MaybeStaticStr<'a>, log::kv::Value<'a>); N],
    log_level: Option<log::Level>,
){
    let Some(tracer) = globals::recorder() else { return };
    match current_span() {
        Some(SpanRef::Local(span)) => {
            let args = SpanEventArgs{ name, occurs_at: Instant::now(), attributes: &attributes(), log_level };
            tracer.add_span_event(&span, args);
        }
        Some(SpanRef::Remote(remote_span)) => tracer.stray_attributes(Some(&remote_span), &attributes()),
        None => tracer.stray_attributes(None, &attributes())
    }
}

pub async fn span_attributes<'a, const N: usize, T>(
    attributes: impl FnOnce() -> [(MaybeStaticStr<'a>, log::kv::Value<'a>); N],
){
    let Some(tracer) = globals::recorder() else { return };
    match current_span() {
        Some(SpanRef::Local(span)) => tracer.set_span_attributes(&span, &attributes()),
        Some(SpanRef::Remote(remote_span)) => tracer.stray_attributes(Some(&remote_span), &attributes()),
        None => tracer.stray_attributes(None, &attributes())
    }
}

#[macro_export]
macro_rules! in_new_span {
    (parent: $parent:expr, $name:tt $(, $attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?)* ; $body:expr) => {
        $crate::in_new_span(
            $crate::internal::MaybeStaticStr::Static($crate::log::__log_key!($name)),
            $parent,
            // $crate::__parent_span!($($parent)*),
            || [$(
                ( // (k,v) tuple
                    $crate::internal::MaybeStaticStr::Static($crate::log::__log_key!($attr_key)),
                    $crate::log::__log_value!($attr_key $(:$capture)* = $($attr_val)*)
                ),
            )*],
            None,
            $body
        )
    };
    ($name:tt $(, $attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?)* ; $body:expr) => {
        $crate::in_new_span!(parent: $crate::current_span(), $name $(, $attr_key $(:$capture)? $(= $attr_val)?)*; $body)
    }
}

#[macro_export]
macro_rules! sync_in_new_span {
    (parent: $parent:expr, $name:tt $(, $attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?)* ; $body:expr) => {
        $crate::sync_in_new_span(
            $crate::internal::MaybeStaticStr::Static($crate::log::__log_key!($name)),
            $parent,
            || [$(
                ( // (k,v) tuple
                    $crate::internal::MaybeStaticStr::Static($crate::log::__log_key!($attr_key)),
                    $crate::log::__log_value!($attr_key $(:$capture)* = $($attr_val)*)
                ),
            )*],
            None,
            || $body
        )
    };
    ($name:tt $(, $attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?)* ; $body:expr) => {
        $crate::sync_in_new_span!(parent: $crate::current_span(), $name $(, $attr_key $(:$capture)? $(= $attr_val)?)*; $body)
    }
}

#[macro_export]
macro_rules! span_attributes {
    ($($attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?),* $(,)?) => {
        $crate::span_attributes(
            || [$(
                ( // (k,v) tuple
                    $crate::internal::MaybeStaticStr::Static($crate::log::__log_key!($attr_key)),
                    $crate::log::__log_value!($attr_key $(:$capture)* = $($attr_val)*)
                ),
            )*],
        )
    };
}

#[macro_export]
macro_rules! span_event {
    ($(error: $error:expr,)? $name:literal, $($key:tt $(:$capture:tt)? $(= $value:expr)?),*) => {
        $crate::span_event(
            $crate::static_str($name),
            $($error || )* false,
            None,
            || [$(
                ($crate::__attr_key!($key), $crate::__attr_value!($key $(:$capture)* = $($value)*))
            ),*],
        )
    };
    ($(error: $error:expr,)? $name:literal) => {
        $crate::span_event(
            $crate::static_str($name),
            $($error || )* false,
            None,
            || vec![],
        )
    };
}

#[tokio::test]
async fn expand_in_new_span(){
    in_new_span!(foo, a=20; async {}).await;
    in_new_span!(foo, a:% =20; async {}).await;
    in_new_span!(foo, a:% = 20 , b:serde = (); async {}).await;

    in_new_span!(parent: None, foo; async {}).await;
    in_new_span!(parent: None, foo, a:% = 20 , b:serde = (); async {}).await;

    let fut = async {};
    in_new_span!(foo, a:% = 20 , b:serde = (); fut).await;
}

#[test]
fn expand_sync_in_new_span(){
    sync_in_new_span!(foo, a=20; {});
    sync_in_new_span!(foo, a:% =20; {});
    sync_in_new_span!(foo, a:% = 20 , b:serde = (); {});

    sync_in_new_span!(parent: None, foo; {});
    sync_in_new_span!(parent: None, foo, a:% = 20 , b:serde = (); {});
}

// #[doc(hidden)]
// #[macro_export]
// macro_rules! __attr_key {
//     // identifier
//     ($arg:ident) => { $crate::internal::MaybeStaticStr::Static(stringify!($arg)) };
//     // (expr)
//     // (($arg:expr)) => { $crate::internal::CompactString::from($arg) };
//     // static str
//     ($arg:literal)  => { $crate::internal::MaybeStaticStr::Static($arg) };
// }

// #[doc(hidden)]
// #[macro_export]
// macro_rules! __attr_value {
//     // NOTE $key will not be used (we need to select between arg or trailing `=` as generated by previous macros)
//     // ($key:tt = $args:expr)              => { $crate::__attr_value!(@($args):value) };
//     ($key:ident =)                      => { $crate::__attr_value!(@($key):value) };
//     ($key:ident :$capture:tt =)         => { $crate::__attr_value!(@($key):$capture) };
//     ($key:tt = $args:expr)              => { $crate::__attr_value!(@($args):value) };
//     ($key:tt :$capture:tt = $args:expr) => { $crate::__attr_value!(@($args):$capture) };

//     (@($args:expr):value)   => { $crate::AttributeValue::from($args) };
//     (@($args:expr):%)       => { $crate::AttributeValue::from($crate::compact_str::ToCompactString::to_compact_string(&$args)) };
//     (@($args:expr):?)       => { $crate::AttributeValue::from(format!("{:?}", $args)) };

//     // (@($args:expr):err)   => { $crate::AttributeValue::from(format!("{:?}", $args)) };
//     // (@($args:expr):serde) => { $crate::AttributeValue::from($crate::serde_json::to_string(&$args).unwrap()) };
// }