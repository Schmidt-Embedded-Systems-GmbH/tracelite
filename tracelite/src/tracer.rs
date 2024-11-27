use std::{num::{NonZeroU128, NonZeroU64}, time::SystemTime};
use crate::{AttributeValue, Severity};

// ++++++++++++++++++++ ids ++++++++++++++++++++

// NOTE uses big-endian to store 16 bytes as u128
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TraceId(pub NonZeroU128);

impl std::fmt::Display for TraceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:032x}", self.0.get())
    }
}

impl std::str::FromStr for TraceId {
    type Err = &'static str;
    fn from_str(hex: &str) -> Result<Self, Self::Err> {
        let mut bytes = [0; 16];
        if hex.len() != bytes.len() * 2 {
            return Err("trace-id string has wrong length")
        }
        for (i, byte) in bytes.iter_mut().enumerate() {
            *byte = match u8::from_str_radix(&hex[i * 2..i * 2 + 2], 16) {
                Ok(b) => b,
                Err(_) => return Err("trace-id string contains non-hex letters")
            }
        }
        let int = u128::from_be_bytes(bytes);
        NonZeroU128::new(int).map(Self).ok_or("trace-id string has all 0 values")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanId(pub NonZeroU64);

impl std::fmt::Display for SpanId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:016x}", self.0.get())
    }
}

impl std::str::FromStr for SpanId {
    type Err = &'static str;
    fn from_str(hex: &str) -> Result<Self, Self::Err> {
        let mut bytes = [0; 8];
        if hex.len() != bytes.len() * 2 {
            return Err("span-id string has wrong length")
        }
        for (i, byte) in bytes.iter_mut().enumerate() {
            *byte = match u8::from_str_radix(&hex[i * 2..i * 2 + 2], 16) {
                Ok(b) => b,
                Err(_) => return Err("span-id string contains non-hex letters")
            }
        }
        let int = u64::from_be_bytes(bytes);
        NonZeroU64::new(int).map(Self).ok_or("span-id string has all 0 values")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanCollectionIndex(pub u32, pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RemoteSpanRef {
    pub trace_id: TraceId,
    pub span_id: SpanId,
    pub trace_flags: u8,
}

impl std::fmt::Display for RemoteSpanRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "00-{}-{}-{:02x}", self.trace_id, self.span_id, self.trace_flags)
    }
}

impl std::str::FromStr for RemoteSpanRef {
    type Err = &'static str;

    fn from_str(header: &str) -> Result<Self, Self::Err> {
        let mut parts = header.split('-');

        let Some(version) = parts.next() else { return Err("missing '-' delimiters") };
        if version != "00" { return Err("unsupported version") }

        let Some(trace_id_hex) = parts.next() else { return Err("missing trace-id field") };
        let Some(span_id_hex) = parts.next() else { return Err("missing parent-id field") };
        let Some(trace_flags_hex) = parts.next() else { return Err("missing trace-flags field") };

        let trace_id = TraceId::from_str(trace_id_hex)?;
        let span_id = SpanId::from_str(span_id_hex)?;

        if trace_flags_hex.len() != 2 { return Err("trace-flags string has wrong length")}
        let trace_flags = u8::from_str_radix(trace_flags_hex, 16)
            .map_err(|_| "trace-flags string contains non-hex letters")?;

        Ok(Self { trace_id, span_id, trace_flags })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSpanRef {
    pub trace_id: TraceId,
    pub span_id: SpanId,
    pub collect_idx: SpanCollectionIndex,
}

// ++++++++++++++++++++ MaybeStaticStr ++++++++++++++++++++

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MaybeStaticStr<'a> {
    Static(&'static str),
    Borrowed(&'a str),
}

impl From<&'static str> for MaybeStaticStr<'static> {
    fn from(s: &'static str) -> Self { Self::Static(s) }
}

impl<'a> std::ops::Deref for MaybeStaticStr<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        match self {
            MaybeStaticStr::Static(s) => &**s,
            MaybeStaticStr::Borrowed(s) => &**s,
        }
    }
}

// ++++++++++++++++++++ PrivateMarker ++++++++++++++++++++

pub struct PrivateMarker(());

// ++++++++++++++++++++ span/event args ++++++++++++++++++++

pub type AttributeListRef<'a> = &'a [(MaybeStaticStr<'a>, AttributeValue<'a>)];
pub type AttributeList<'a, const N: usize> = [(MaybeStaticStr<'a>, AttributeValue<'a>); N];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SpanAncestor {
    Local(LocalSpanRef),
    Remote(RemoteSpanRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SpanRef {
    Recording(LocalSpanRef),
    // NOTE span is not recording (head-based sampling), but got a recording or remote SpanAncestor
    NotRecording(Option<SpanAncestor>),
    Remote(RemoteSpanRef),
}

impl From<LocalSpanRef> for SpanRef {
    fn from(s: LocalSpanRef) -> Self { Self::Recording(s) }
}

impl From<RemoteSpanRef> for SpanRef {
    fn from(s: RemoteSpanRef) -> Self { Self::Remote(s) }
}

impl SpanRef {
    pub fn trace_and_span_id(self) -> Option<(TraceId, SpanId)> {
        match self {
            Self::Recording(span) => Some((span.trace_id, span.span_id)),
            Self::NotRecording(ancestor) => match ancestor? {
                SpanAncestor::Local(span) => Some((span.trace_id, span.span_id)),
                SpanAncestor::Remote(span) => Some((span.trace_id, span.span_id)),
            }
            Self::Remote(span) => Some((span.trace_id, span.span_id)),
        }
    }
    pub fn trace_id(self) -> Option<TraceId> { self.trace_and_span_id().map(|(t, _)| t) }
    pub fn span_id(self) -> Option<SpanId> { self.trace_and_span_id().map(|(_, s)| s) }

    pub fn to_remote(self) -> Option<RemoteSpanRef> {
        match self {
            SpanRef::Recording(span) => Some(RemoteSpanRef{ trace_id: span.trace_id, span_id: span.span_id, trace_flags: 1 }),
            SpanRef::NotRecording(_) => None,
            SpanRef::Remote(span) => Some(span)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanStatus<'a> {
    Ok,
    Error(MaybeStaticStr<'a>),
}

impl<'a> SpanStatus<'a> {
    pub fn error(s: impl Into<MaybeStaticStr<'a>>) -> Self {
        Self::Error(s.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanKind {
    Internal,
    Client,
    Server,
    Producer,
    Consumer,
}

#[non_exhaustive]
#[derive(Debug)]
pub struct SpanArgs<'a> {
    pub name: MaybeStaticStr<'a>, 
    pub target: &'a str,
    pub severity: Severity,
    pub parent: Option<SpanRef>,
    pub opened_at: SystemTime,
    pub attributes: AttributeListRef<'a>,
    pub status: Option<SpanStatus<'a>>,
    pub kind: Option<SpanKind>,
}

impl<'a> SpanArgs<'a> {
    // NOTE severity is not a setable field because as tokio tracing discussions revealed, having 
    //      a default severity (log level) is a bad idea - some assume the default to be INFO, others TRACE
    pub fn new(name: impl Into<MaybeStaticStr<'a>>, severity: Severity, target: &'a str) -> Self {
        let name = name.into();
        Self {
            name,
            severity,
            target,
            parent: globals::current_span(), // TODO call this later?
            opened_at: SystemTime::now(),
            attributes: &[],
            kind: None,
            status: (severity >= Severity::Error).then(|| SpanStatus::error(name))
        }
    }
    pub fn build<'b, const N: usize>(self, attributes: AttributeList<'b, N>) -> Option<OwnedSpan> {
        let local = globals::tracer()?.open_span(SpanArgs{ attributes: &attributes, ..self }, PrivateMarker(()));
        Some(OwnedSpan(Some(SpanRef::Recording(local))))
    }

    pub fn parent(self, parent: impl Into<Option<SpanRef>>) -> Self {
        Self{ parent: parent.into(), ..self }
    }
    pub fn name<'b>(self, name: impl Into<MaybeStaticStr<'b>>) -> SpanArgs<'b>
        where 'a: 'b
    {
        SpanArgs{ name: name.into(), ..self }
    }
    pub fn status<'b>(self, status: SpanStatus<'a>) -> SpanArgs<'b>
        where 'a: 'b
    {
        SpanArgs{ status: Some(status), ..self }
    }
    pub fn kind(self, kind: SpanKind) -> Self {
        Self{ kind: Some(kind), ..self }
    }

}

#[non_exhaustive]
#[derive(Debug)]
pub struct EventArgs<'a> {
    pub name: MaybeStaticStr<'a>,
    pub occurs_at: SystemTime,
    pub attributes: AttributeListRef<'a>,
    pub severity: Option<Severity>,
}

impl<'a> EventArgs<'a> {
    pub fn new(name: impl Into<MaybeStaticStr<'a>>) -> Self {
        Self {
            name: name.into(),
            occurs_at: SystemTime::now(),
            attributes: &[],
            severity: None,
        }
    }

    pub fn record<'b, const N: usize>(self, attributes: AttributeList<'b, N>,){
        let Some(t) = globals::tracer() else { return };
        match globals::current_span() {
            Some(SpanRef::Recording(local)) => t.add_event(&local, EventArgs{ attributes: &attributes, ..self }),
            Some(SpanRef::NotRecording(_)) => {}
            Some(SpanRef::Remote(_)) => unreachable!(),
            None => {
                t.instrumentation_error(InstrumentationError::StrayEvent(self));
            }
        }
    }
}

// #[derive(Setters)]
// #[setters(strip_option, prefix="")]
// pub struct ExceptionArgs<'a, E> {
//     #[setters(skip)]
//     pub exception: E,
//     pub error: Option<MaybeStaticStr<'a>>,
//     #[setters(bool)]
//     pub escaped: bool,
//     #[setters(bool)]
//     pub stacktrace: bool,
//     #[setters(skip)]
//     _private: PrivateMarker,
// }

// impl<'a, E> ExceptionArgs<'a, E> {
//     pub fn record(self){
//         if let Some(err_msg) = self.error.as_ref() {
//             globals::set_status(SpanStatus::Error(*err_msg));
//         }
//         EventArgs::new(name)
//             .
//     }
// }

// ++++++++++++++++++++ Tracer ++++++++++++++++++++

#[non_exhaustive]
pub enum InstrumentationError<'a> {
    StrayAttributes(AttributeListRef<'a>),
    StrayEvent(EventArgs<'a>),
    StraySpanStatus(SpanStatus<'a>),
}

impl<'a> std::fmt::Display for InstrumentationError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ERROR] tracelite: instrumentation error: ")?;
        // TODO properly format everything
        match self {
            Self::StrayAttributes(attr) => write!(f, "stray attributes: {:?}", attr),
            Self::StrayEvent(event_args) => write!(f, "stray event: {:?}", event_args),
            Self::StraySpanStatus(span_status) => write!(f, "stray span status: {:?}", span_status),
        }
    }
}

pub trait Tracer: Send + Sync + 'static {
    fn should_sample(&self, severity: Severity, target: &str) -> bool;

    fn open_span(&self, args: SpanArgs, _private: PrivateMarker) -> LocalSpanRef;
    fn set_attributes(&self, span: &LocalSpanRef, attrs: AttributeListRef);
    fn add_event(&self, span: &LocalSpanRef, args: EventArgs);
    fn set_span_status(&self, span: &LocalSpanRef, status: SpanStatus);
    fn close_span(&self, span: &LocalSpanRef, closed_at: SystemTime);
    fn drop_span(&self, idx: SpanCollectionIndex, dropped_at: SystemTime, _private: PrivateMarker);
    fn flush(&self);

    fn instrumentation_error(&self, err: InstrumentationError);
}

// TODO hide variants?

#[derive(Debug)]
pub struct OwnedSpan(Option<SpanRef>);

impl OwnedSpan {
    pub fn get_ref(&self) -> SpanRef { self.0.unwrap() }
}

impl Drop for OwnedSpan {
    fn drop(&mut self) {
        if let Some(SpanRef::Recording(span)) = self.0.take() {
            if let Some(f) = globals::tracer() {
                f.drop_span(span.collect_idx, SystemTime::now(), PrivateMarker(()));
            }
        }
    }
}

pub mod globals {
    use crate::AttributeValue;
    use super::{AttributeList, EventArgs, InstrumentationError, MaybeStaticStr, OwnedSpan, SpanRef, SpanStatus, Tracer};
    use tokio::task::futures::TaskLocalFuture;
    use std::future::Future;

    // TODO does not work - static objects do not get dropped
    struct FlushOnDrop(pub Box<dyn Tracer>);

    impl Drop for FlushOnDrop {
        fn drop(&mut self) {
            self.0.flush();
        }
    }

    static TRACER: std::sync::OnceLock<FlushOnDrop> = std::sync::OnceLock::new();

    tokio::task_local! {
        static CURRENT_SPAN: OwnedSpan;
    }

    pub fn set_tracer(tracer: Box<dyn Tracer>){
        TRACER.set(FlushOnDrop(tracer)).ok()
            .expect("[FATAL] tracelite: tracer already set");
    }

    pub(crate) fn tracer() -> Option<&'static dyn Tracer> {
        TRACER.get().map(|f| &*f.0)
    }

    pub fn current_span() -> Option<SpanRef> {
        CURRENT_SPAN.try_with(|owned| owned.0.clone()).ok().flatten()
    }

    pub enum MaybeInSpan<F>{
        InSpan(TaskLocalFuture<OwnedSpan, F>),
        NotInSpan(F),
    }

    impl<F> std::future::Future for MaybeInSpan<F>
        where F: std::future::Future
    {
        type Output = F::Output;
        fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
            // TODO can we get rid of unsafe?
            match unsafe { std::pin::Pin::get_unchecked_mut(self) } {
                MaybeInSpan::InSpan(f) => unsafe { std::pin::Pin::new_unchecked(f).poll(cx) },
                MaybeInSpan::NotInSpan(f) => unsafe { std::pin::Pin::new_unchecked(f).poll(cx) },
            }
        }
    }

    pub trait InSpan: Future + Sized {
        fn in_span(self, span: impl Into<Option<OwnedSpan>>) -> MaybeInSpan<Self> {
            if let Some(span) = span.into() {
                MaybeInSpan::InSpan(CURRENT_SPAN.scope(span, self))
            } else {
                MaybeInSpan::NotInSpan(self)
            }
        }
    }

    impl<F: Future + Sized> InSpan for F {}

    pub fn sync_in_span<R>(span: impl Into<Option<OwnedSpan>>, f: impl FnOnce() -> R) -> R {
        if let Some(span) = span.into() {
            CURRENT_SPAN.sync_scope(span, f)
        } else {
            f()
        }
    }

    /// NOTE you will likely want to use span_attributes!() instead
    pub fn set_attributes<'a, const N: usize>(attrs: AttributeList<'a, N>) {
        let Some(t) = tracer() else { return };
        match current_span() {
            Some(SpanRef::Recording(span)) => {
                t.set_attributes(&span, &attrs);
            }
            Some(SpanRef::NotRecording(_)) => {}
            Some(SpanRef::Remote(_)) => unreachable!(),
            None => {
                let err = InstrumentationError::StrayAttributes(&attrs);
                t.instrumentation_error(err);
            }
        }
    }

    pub fn set_status(status: SpanStatus<'_>){
        let Some(t) = tracer() else { return };
        match current_span() {
            Some(SpanRef::Recording(span)) => {
                t.set_span_status(&span, status);
            }
            Some(SpanRef::NotRecording(_)) => {}
            Some(SpanRef::Remote(_)) => unreachable!(),
            None => {
                let err = InstrumentationError::StraySpanStatus(status);
                t.instrumentation_error(err);
            }
        }
    }

    pub fn mark_span_as_error<'a>(msg: impl Into<MaybeStaticStr<'a>>){
        set_status(SpanStatus::error(msg));
    }

    pub fn mark_span_as_ok(){
        set_status(SpanStatus::Ok);
    }

    pub fn record_exception(ex: &impl std::error::Error){
        let source = ex.source();
        let source2 = source.as_ref();
        EventArgs::new("exception")
            .record([
                ("exception.message".into(),AttributeValue::DynDisplay(ex)),
                ("exception.type".into(), std::any::type_name_of_val(ex).into()),
                // TODO include source of source, and so on
                ("exception.source".into(), match source2 {
                    Some(source) => AttributeValue::DynDisplay(source) ,
                    None => AttributeValue::NotPresent,
                }),
            ])
    }

    pub fn record_exception_as_error<'a>(ex: &impl std::error::Error, msg: impl Into<MaybeStaticStr<'a>>){
        record_exception(ex);
        mark_span_as_error(msg);
    }

    pub fn record_exception_debug(ex: &impl std::fmt::Debug){
        EventArgs::new("exception")
            .record([
                ("exception.message".into(), AttributeValue::DynDebug(ex)),
                ("exception.type".into(), std::any::type_name_of_val(ex).into()),
            ])
    }

    pub fn record_exception_debug_as_error<'a>(ex: &impl std::fmt::Debug, msg: impl Into<MaybeStaticStr<'a>>){
        record_exception_debug(ex);
        mark_span_as_error(msg);
    }

    pub trait RecordException<E> {
        fn record(self) -> Self
            where E: std::error::Error;
        fn record_as_error<'a>(self, msg: impl Into<MaybeStaticStr<'a>>) -> Self
            where E: std::error::Error;

        fn record_debug(self) -> Self
            where E: std::fmt::Debug;
        fn record_debug_as_error<'a>(self, msg: impl Into<MaybeStaticStr<'a>>) -> Self
            where E: std::fmt::Debug;
    }

    impl<T, E> RecordException<E> for Result<T, E> {
        fn record(self) -> Self
            where E: std::error::Error
        {
            self.inspect_err(|ex| record_exception(&ex))
        }
        fn record_as_error<'a>(self, msg: impl Into<MaybeStaticStr<'a>>) -> Self
            where E: std::error::Error
        {
            self.inspect_err(|ex| record_exception_as_error(&ex, msg.into()))
        }
        fn record_debug(self) -> Self
            where E: std::fmt::Debug
        {
            self.inspect_err(|ex| record_exception_debug(&ex))
        }
        
        fn record_debug_as_error<'a>(self, msg: impl Into<MaybeStaticStr<'a>>) -> Self
            where E: std::fmt::Debug
        {
            self.inspect_err(|ex| record_exception_debug_as_error(&ex, msg.into()))
        }
    }
}