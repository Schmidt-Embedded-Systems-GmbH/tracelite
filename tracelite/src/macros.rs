
#[macro_export]
macro_rules! __attr_key {
    ($($ident:ident).+) => {
        $crate::Text::Static(stringify!( $($ident).+ ))
    };
    ($str:literal) => {
        $crate::Text::Static($str)
    };
}

#[macro_export]
macro_rules! __attr_muncher {

    /* initial case or trailing-comma case */
    (@out{ $(, $out:expr)* } , $($rest:tt)*) => {
        $crate::__attr_muncher!( @out{ $(, $out)* } $($rest)* )
    };

    /* final case (emits $out) */
    (@out{ $(, $out:expr)* } ) => {
        [ $($out),* ]
    };

    /* from display */
    (@out{ $(, $out:expr)* } % $key:tt = $val:expr $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($key), $crate::AttributeValue::display(&$val))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };
    (@out{ $(, $out:expr)* } % $var:ident $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($var), $crate::AttributeValue::display(&$var))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };

    /* from debug */
    (@out{ $(, $out:expr)* } ? $key:tt = $val:expr $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($key), $crate::AttributeValue::debug(&$val))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };
    (@out{ $(, $out:expr)* } ? $var:ident $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($var), $crate::AttributeValue::debug(&$var))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };

    /* from serialize */
    (@out{ $(, $out:expr)* } # $key:tt = $val:expr $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($key), $crate::AttributeValue::serialize(&$val))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };
    (@out{ $(, $out:expr)* } # $var:ident $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($var), $crate::AttributeValue::serialize(&$var))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };

    /* from value */
    (@out{ $(, $out:expr)* } $key:tt = $val:expr $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($key), $crate::AttributeValue::from($val))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };
    (@out{ $(, $out:expr)* } $var:ident $(, $($rest:tt)* )? ) => {
        $crate::__attr_muncher!(
            @out{
                , ($crate::__attr_key!($var), $crate::AttributeValue::from($var))
                $(, $out)* 
            }
            $( $($rest)* )?
        )
    };
}

#[macro_export]
macro_rules! __new_span {
    ($severity:expr, $name:literal $(,$($rest:tt)*)?) => {
        if let Ok(tracer) = $crate::tracer() {
            let target = Some(std::module_path!());
            let severity = $severity;

            if tracer.is_enabled(target, severity) {
                let mut _span_args = $crate::SpanBuilder::new($crate::Text::from($name), target, severity);
                Some($crate::__new_span!(@munch(tracer; _span_args) $($($rest)*)?))
            } else {
                None
            }
        } else {
            None
        }
    };
    (@munch($tracer:ident; $args:ident) $setter:ident( $($setter_args:expr),* $(,)? )  $(,$($rest:tt)*)? ) => {
        {
            // yes, we need to expand it in this absurd way to allow for use of format_args!()
            (|_span_args: $crate::SpanBuilder|{
                $crate::__new_span!(@munch($tracer; _span_args) $($($rest)*)?)
            })($args.$setter($($setter_args),*))
        }
    };
    (@munch($tracer:ident; $args:ident) $($attrs:tt)* ) => {
        $args.start($tracer, $crate::__attr_muncher!(@out{} $($attrs)*) )
    }
}

#[macro_export] macro_rules! new_span        { ($name:literal $($rest:tt)*) => { $crate::__new_span!(None, $name $($rest)*) } }
#[macro_export] macro_rules! new_trace_span  { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Trace ), $name $($rest)*) } }
#[macro_export] macro_rules! new_trace2_span { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Trace ), $name $($rest)*) } }
#[macro_export] macro_rules! new_debug_span  { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Debug ), $name $($rest)*) } }
#[macro_export] macro_rules! new_debug2_span { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Debug2), $name $($rest)*) } }
#[macro_export] macro_rules! new_info_span   { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Info  ), $name $($rest)*) } }
#[macro_export] macro_rules! new_info2_span  { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Info2 ), $name $($rest)*) } }
#[macro_export] macro_rules! new_warn_span   { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Warn  ), $name $($rest)*) } }
#[macro_export] macro_rules! new_warn2_span  { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Warn2 ), $name $($rest)*) } }
#[macro_export] macro_rules! new_error_span  { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Error ), $name $($rest)*) } }
#[macro_export] macro_rules! new_error2_span { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Error2), $name $($rest)*) } }
#[macro_export] macro_rules! new_fatal_span  { ($name:literal $($rest:tt)*) => { $crate::__new_span!(Some($crate::Severity::Fatal ), $name $($rest)*) } }

#[macro_export]
macro_rules! span_attributes {
    ($($attrs:tt)*) => {
        {
            $crate::set_attributes($crate::__attr_muncher!(@out{} $($attrs)*) )
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __new_event {
    ($severity:expr, $name:literal $(,$($rest:tt)*)?) => {
        if let Ok(tracer) = $crate::tracer() {
            let target = Some(std::module_path!());
            let severity = $severity;

            if tracer.is_enabled(target, severity) {
                let mut _event_args = $crate::EventBuilder::new($crate::Text::from($name), target, severity);
                $crate::__new_event!(@munch(tracer; _event_args) $($($rest)*)?);
            }
        }
    };
    (@munch($tracer:ident; $args:ident) $setter:ident( $($setter_args:expr)* )  $(,$($rest:tt)*)? ) => {
        // yes, we need to expand it in this absurd way to allow for use of format_args!()
        (|_event_args: $crate::EventArgs|{
            $crate::__new_event!(@munch($tracer; _event_args) $($($rest)*)?)
        })($args.$setter($($setter_args),*))
    };
    (@munch($tracer:ident; $args:ident) $($attrs:tt)* ) => {
        $args.add_to_current_span($tracer, $crate::__attr_muncher!(@out{} $($attrs)*) );
    }
}

#[macro_export] macro_rules! event        { ($name:literal $($rest:tt)*) => { $crate::__new_event!(None, $name $($rest)*) } }
#[macro_export] macro_rules! trace_event  { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Trace ), $name $($rest)*) } }
#[macro_export] macro_rules! trace2_event { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Trace2), $name $($rest)*) } }
#[macro_export] macro_rules! debug_event  { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Debug ), $name $($rest)*) } }
#[macro_export] macro_rules! debug2_event { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Debug2), $name $($rest)*) } }
#[macro_export] macro_rules! info_event   { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Info  ), $name $($rest)*) } }
#[macro_export] macro_rules! info2_event  { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Info2 ), $name $($rest)*) } }
#[macro_export] macro_rules! warn_event   { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Warn  ), $name $($rest)*) } }
#[macro_export] macro_rules! warn2_event  { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Warn2 ), $name $($rest)*) } }
#[macro_export] macro_rules! error_event  { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Error ), $name $($rest)*) } }
#[macro_export] macro_rules! error2_event { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Error2), $name $($rest)*) } }
#[macro_export] macro_rules! fatal_event  { ($name:literal $($rest:tt)*) => { $crate::__new_event!(Some($crate::Severity::Fatal ), $name $($rest)*) } }


#[test]
fn test_macro_expansion(){

    let a = 2;
    new_info_span!(
        "overwritten_name"
        , name("real_name")
        , %a
        , a
        // @kind(crate::SpanKind::Internal),
        // , a
        // a,
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
    let foobar = 5;
    span_attributes!(foo = 2, bar = 3, foobar, %foobar, ?foobar, #foobar, ?foobar = [1,2]);
}