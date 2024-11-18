use std::{num::{NonZeroU128, NonZeroU64}, time::SystemTime};
use crate::Severity;

// ++++++++++++++++++++ ids ++++++++++++++++++++

// TODO in what endianess are bytes stored?
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TraceId(pub NonZeroU128);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanId(pub NonZeroU64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanCollectionIndex(pub u32, pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RemoteSpanRef {
    pub trace_id: TraceId,
    pub span_id: SpanId,
}

// TOOD make parts of this non-pub?
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

// TODO get rid of this entirely?
pub struct PrivateMarker(());

// ++++++++++++++++++++ span/event args ++++++++++++++++++++

pub type AttributeListRef<'a> = &'a [(MaybeStaticStr<'a>, log::kv::Value<'a>)];
pub type AttributeListFixedSize<'a, const N: usize> = [(MaybeStaticStr<'a>, log::kv::Value<'a>); N];
// TODO use plain fixed array with sentinel values?
// pub type InitAttributeList<'a> = [(Key<'a>, log::kv::Value<'a>); 16];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SpanParent {
    Local(LocalSpanRef),
    Remote(RemoteSpanRef),
}

impl From<LocalSpanRef> for SpanParent {
    fn from(s: LocalSpanRef) -> Self { Self::Local(s) }
}
impl From<RemoteSpanRef> for SpanParent {
    fn from(s: RemoteSpanRef) -> Self { Self::Remote(s) }
}

impl SpanParent {
    pub fn trace_id(&self) -> TraceId {
        match self {
            Self::Local(span) => span.trace_id,
            Self::Remote(span) => span.trace_id,
        }
    }
    pub fn span_id(&self) -> SpanId {
        match self {
            Self::Local(span) => span.span_id,
            Self::Remote(span) => span.span_id,
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
pub struct SpanArgs<'a> {
    pub name: MaybeStaticStr<'a>, 
    pub severity: Severity,
    pub parent: Option<SpanParent>,
    pub opened_at: SystemTime,
    pub attributes: AttributeListRef<'a>,
    pub status: Option<SpanStatus<'a>>, // TODO add Unset instead
    pub kind: Option<SpanKind>,
}

impl<'a> SpanArgs<'a> {
    // NOTE severity is not a setable field because as tokio tracing discussions revealed, having 
    //      a default severity (log level) is a bad idea - some assume the default to be INFO, others TRACE
    pub fn new(name: impl Into<MaybeStaticStr<'a>>, severity: Severity) -> Self {
        let name = name.into();
        Self {
            name,
            severity,
            // TODO is there performance overhead here if we call parent() afterwards?
            parent: globals::current_span().and_then(|span| match span {
                Span::Recording(span) => Some(span.into()),
                Span::NotRecording(parent) => parent,
            }),
            opened_at: SystemTime::now(),
            attributes: &[],
            kind: None,
            status: (severity >= Severity::Error).then(|| SpanStatus::error(name))
        }
    }
    pub fn build<'b>(self, attributes: AttributeListRef<'b>) -> Option<OwnedSpan> {
        let local = globals::tracer()?.open_span(SpanArgs{ attributes, ..self }, PrivateMarker(()));
        Some(OwnedSpan(Some(Span::Recording(local))))
    }

    pub fn parent(self, parent: impl Into<Option<SpanParent>>) -> Self {
        Self{ parent: parent.into(), ..self }
    }
    pub fn status(self, status: SpanStatus<'a>) -> Self {
        Self{ status: Some(status), ..self }
    }
    pub fn kind(self, kind: SpanKind) -> Self {
        Self{ kind: Some(kind), ..self }
    }

}

#[non_exhaustive]
pub struct EventArgs<'a> {
    pub name: MaybeStaticStr<'a>,
    pub occurs_at: SystemTime,
    pub attributes: AttributeListRef<'a>,
    pub sample_level: Option<Severity>,
}

impl<'a> EventArgs<'a> {
    pub fn new(name: impl Into<MaybeStaticStr<'a>>) -> Self {
        Self {
            name: name.into(),
            occurs_at: SystemTime::now(),
            attributes: &[],
            sample_level: None,
        }
    }

    pub fn record<'b>(self, attributes: AttributeListRef<'b>,){
        let Some(t) = globals::tracer() else { return };
        match globals::current_span() {
            Some(Span::Recording(local)) => t.add_event(&local, EventArgs{ attributes, ..self }),
            Some(Span::NotRecording(_)) => {}
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
            Self::StrayAttributes(attr) => write!(f, "stray attributes {:?}", attr),
            Self::StrayEvent(event_args) => write!(f, "stray event {:?}", event_args.name),
            Self::StraySpanStatus(span_status) => write!(f, "stray span status: {:?}", span_status),
        }
    }
}

pub trait Tracer: Send + Sync + 'static {
    fn open_span(&self, args: SpanArgs, _private: PrivateMarker) -> LocalSpanRef;
    fn set_attributes(&self, span: &LocalSpanRef, attrs: AttributeListRef);
    fn add_event(&self, span: &LocalSpanRef, args: EventArgs);
    fn set_span_status(&self, span: &LocalSpanRef, status: SpanStatus);
    fn close_span(&self, span: &LocalSpanRef, closed_at: SystemTime);
    fn drop_span(&self, idx: SpanCollectionIndex, dropped_at: SystemTime, _private: PrivateMarker);
    fn flush(&self);

    fn instrumentation_error(&self, err: InstrumentationError);
}

#[derive(Clone)]
pub enum Span {
    Recording(LocalSpanRef),
    NotRecording(Option<SpanParent>),
}

pub struct OwnedSpan(Option<Span>);

impl Drop for OwnedSpan {
    fn drop(&mut self) {
        if let Some(Span::Recording(span)) = self.0.take() {
            if let Some(f) = globals::tracer() {
                f.drop_span(span.collect_idx, SystemTime::now(), PrivateMarker(()));
            }
        }
    }
}

pub mod globals {
    use super::{AttributeListFixedSize, Span, EventArgs, InstrumentationError, MaybeStaticStr, OwnedSpan, SpanStatus, Tracer};
    use tokio::task::futures::TaskLocalFuture;
    use std::future::Future;

    static TRACER: std::sync::OnceLock<Box<dyn Tracer>> = std::sync::OnceLock::new();

    tokio::task_local! {
        static CURRENT_SPAN: OwnedSpan;
    }

    pub fn set_tracer(tracer: Box<dyn Tracer>){
        TRACER.set(tracer).ok()
            .expect("[FATAL] tracelite: tracer already set");
    }

    pub(crate) fn tracer() -> Option<&'static dyn Tracer> {
        TRACER.get().map(|f| &**f)
    }

    pub fn current_span() -> Option<Span> {
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

    pub fn in_span<R>(span: impl Into<Option<OwnedSpan>>, f: impl FnOnce() -> R) -> R {
        if let Some(span) = span.into() {
            CURRENT_SPAN.sync_scope(span, f)
        } else {
            f()
        }
    }

    /// NOTE you will likely want to use span_attributes!() instead
    // TODO do we need FixedSize here?
    pub fn set_attributes<'a, const N: usize>(attrs: AttributeListFixedSize<'a, N>){
        let Some(t) = tracer() else { return };
        match current_span() {
            Some(Span::Recording(span)) => {
                t.set_attributes(&span, &attrs);
            }
            Some(Span::NotRecording(_)) => {}
            None => {
                let err = InstrumentationError::StrayAttributes(&attrs);
                t.instrumentation_error(err);
            }
        }
    }

    pub fn set_status(status: SpanStatus<'_>){
        let Some(t) = tracer() else { return };
        match current_span() {
            Some(Span::Recording(span)) => {
                t.set_span_status(&span, status);
            }
            Some(Span::NotRecording(_)) => {}
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
            .record(&[
                ("exception.message".into(), log::kv::Value::from_display(ex)),
                ("exception.type".into(), std::any::type_name_of_val(ex).into()),
                // TODO include source of source, and so on
                ("exception.source".into(), match source2 {
                    Some(source) => log::kv::Value::from_display(source) ,
                    None => log::kv::Value::null()
                }),
            ])
    }

    pub fn record_exception_as_error<'a>(ex: &impl std::error::Error, msg: impl Into<MaybeStaticStr<'a>>){
        record_exception(ex);
        mark_span_as_error(msg);
    }

    pub trait RecordException {
        fn record(self) -> Self;
        fn record_as_error<'a>(self, msg: impl Into<MaybeStaticStr<'a>>) -> Self;
    }

    impl<T, E> RecordException for Result<T, E>
        where E: std::error::Error
    {
        fn record(self) -> Self {
            self.map_err(|ex| {
                record_exception(&ex);   
                ex
            })
        }
        fn record_as_error<'a>(self, msg: impl Into<MaybeStaticStr<'a>>) -> Self {
            self.map_err(|ex| {
                record_exception_as_error(&ex, msg.into());   
                ex
            })
        }
    }
}