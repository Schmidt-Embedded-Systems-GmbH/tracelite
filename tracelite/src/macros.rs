

#[macro_export]
macro_rules! __new_span {
    ($severity:expr, $name:literal
        $($(,)? [ $( $setter:ident ( $($setter_args:expr)* ) ),* $(,)? ])?
        $(, $attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?)*
        $(,)*
    ) => {
        {
            let mut _span = $crate::SpanArgs::new($crate::MaybeStaticStr::Static($name), $severity);
            $($(
                _span = _span.$setter($($setter_args),*);
            )*)?
            _span.build(
                &[
                    $(
                        (
                            $crate::MaybeStaticStr::Static($crate::log::__log_key!($attr_key)),
                            $crate::log::__log_value!($attr_key $(:$capture)* = $($attr_val)?)
                        ),
                    )*
                ]
            )
        }
    }
}

#[macro_export]
macro_rules! new_trace_span {
    ($name:literal $($rest:tt)*) => { $crate::__new_span!(($crate::Severity::Trace), $name $($rest)*) }
}
#[macro_export]
macro_rules! new_debug_span {
    ($name:literal $($rest:tt)*) => { $crate::__new_span!(($crate::Severity::Debug), $name $($rest)*) }
}
#[macro_export]
macro_rules! new_info_span {
    ($name:literal $($rest:tt)*) => { $crate::__new_span!(($crate::Severity::Info), $name $($rest)*) }
}

#[macro_export]
macro_rules! span_attributes {
    ( $($attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?),* $(,)* ) => {
        {
            $crate::set_attributes([
                $(
                    ((
                        $crate::tracer::MaybeStaticStr::Static($crate::log::__log_key!($attr_key)),
                        $crate::log::__log_value!($attr_key $(:$capture)* = $($attr_val)*)
                    )),
                )*
            ])
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __new_event {
    ($severity:expr, $name:literal
        $($(,)? [ $( $setter:ident ( $($setter_args:expr)* ) ),* $(,)? ])?
        $(, $attr_key:tt $(:$capture:tt)? $(= $attr_val:expr)?)*
        $(,)*
    ) => {
        {
            let mut _event = $crate::EventArgs::new($crate::MaybeStaticStr::Static($name));
            _event.severity = $severity;
            $($(
                _event = _event.$setter($($setter_args),*);
            )*)?
            _event.record(
                &[
                    $(
                        (
                            $crate::MaybeStaticStr::Static($crate::log::__log_key!($attr_key)),
                            $crate::log::__log_value!($attr_key $(:$capture)* = $($attr_val)?)
                        ),
                    )*
                ]
            )
        }
    }
}

#[macro_export]
macro_rules! event {
    ($name:literal $($rest:tt)*) => { $crate::__new_event!(None, $name $($rest)*) }
}
#[macro_export]
macro_rules! trace_event {
    ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Trace), $name $($rest)*) }
}
#[macro_export]
macro_rules! debug_event {
    ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Debug), $name $($rest)*) }
}
#[macro_export]
macro_rules! info_event {
    ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Info ), $name$($rest)*) }
}
#[macro_export]
macro_rules! warn_event {
    ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Warn ), $name$($rest)*) }
}
#[macro_export]
macro_rules! error_event {
    ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Error), $name$($rest)*) }
}


#[test]
fn test_macro_expansion(){

    let a = 2;
    new_info_span!(
        "overwritten_name"
        [kind(crate::SpanKind::Internal)],
        a,
        // a : % = 1 
        // name("real_name"),
    );

    // new_span!(
    //     "overwritten_name",
    //     attr(a:% = 1 , b:? = vec![2]),
    //     name("real_name"),
    //     kind(SpanKind::Server)
    // );

    // new_span!(
    //     "overwritten_name",
    //     name("real_name"),
    //     kind(SpanKind::Server)
    // );
}


#[test]
fn test_expand_span_attributes(){
    // crate::add_attributes(|| [("foo".into(), 2.into())]);
    span_attributes!(foo :% = 2, bar = 3, xoxo :serde = vec![1,2,3]);

    // span_event!("foobar", info, attr(x:% = 2))
}