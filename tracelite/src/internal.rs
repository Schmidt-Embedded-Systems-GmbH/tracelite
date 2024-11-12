use std::{num::{NonZeroU128, NonZeroU64}, time::SystemTime};

// ++++++++++++++++++++ ids ++++++++++++++++++++
// TODO should we make everything `pub` here?

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSpanRef {
    pub trace_id: TraceId,
    pub span_id: SpanId,
    pub collect_idx: SpanCollectionIndex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum SpanRef_ {
    Local(LocalSpanRef),
    Remote(RemoteSpanRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanRef(SpanRef_);

impl From<LocalSpanRef> for SpanRef {
    fn from(s: LocalSpanRef) -> Self { Self(SpanRef_::Local(s)) }
}
impl From<RemoteSpanRef> for SpanRef {
    fn from(s: RemoteSpanRef) -> Self { Self(SpanRef_::Remote(s)) }
}

impl SpanRef {
    pub fn trace_id(&self) -> TraceId {
        match self.0 {
            SpanRef_::Local(span) => span.trace_id,
            SpanRef_::Remote(span) => span.trace_id,
        }
    }
    pub fn span_id(&self) -> SpanId {
        match self.0 {
            SpanRef_::Local(span) => span.span_id,
            SpanRef_::Remote(span) => span.span_id,
        }
    }
    // fn collect_idx(&self) -> Option<SpanCollectionIndex> {
    //     match self.0 {
    //         SpanRef_::Local(span) => Some(span.collect_idx),
    //         SpanRef_::Remote(_) => None
    //     }
    // }
}

// ++++++++++++++++++++ Key ++++++++++++++++++++

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Key<'a> {
    Static(&'static str),
    Borrowed(&'a str),
}

impl From<&'static str> for Key<'static> {
    fn from(s: &'static str) -> Self { Self::Static(s) }
}

impl<'a> std::ops::Deref for Key<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        match self {
            Key::Static(s) => &**s,
            Key::Borrowed(s) => &**s,
        }
    }
}

// ++++++++++++++++++++ PrivateMarker ++++++++++++++++++++

pub struct PrivateMarker(());

// ++++++++++++++++++++ span/event args ++++++++++++++++++++

pub type AttributeListRef<'a> = &'a [(Key<'a>, log::kv::Value<'a>)];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanKind {
    Internal,
    Client,
    Server,
    Producer,
    Consumer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanStatus {
    Ok,
    Error,
}

#[derive(Setters)]
#[setters(strip_option, prefix="")]
pub struct SpanArgs<'a> {
    pub name: Key<'a>,
    #[setters(strip_option=false)]
    #[setters(into)]
    pub parent: Option<SpanRef>,
    #[setters(skip)]
    pub opened_at: SystemTime,
    pub init_attributes: AttributeListRef<'a>,
    pub log_level: Option<log::Level>,
    // pub sampled: Option<bool>,
    pub span_kind: Option<SpanKind>,
    pub span_status: Option<SpanStatus>,
    #[setters(skip)]
    _private: PrivateMarker,
}

impl<'a> SpanArgs<'a> {
    pub fn new(name: Key<'a>) -> Self {
        Self {
            name,
            parent: globals::current_span(),
            opened_at: SystemTime::now(),
            init_attributes: &[],
            log_level: None,
            // sampled: None,
            span_kind: None,
            span_status: None,
            _private: PrivateMarker(())
        }
    }
    pub fn build(self) -> Option<SpanGuard> {
        let local = globals::tracer()?.open_span(self, PrivateMarker(()));
        Some(SpanGuard(Some(SpanRef(SpanRef_::Local(local)))))
    }
}

#[derive(Setters)]
#[setters(strip_option, prefix="")]
pub struct EventArgs<'a> {
    pub name: Key<'a>,
    #[setters(skip)]
    pub occurs_at: SystemTime,
    pub attributes: AttributeListRef<'a>,
    pub log_level: Option<log::Level>,
    #[setters(skip)]
    _private: PrivateMarker,
}

impl<'a> EventArgs<'a> {
    pub fn new(name: Key<'a>) -> Self {
        Self {
            name,
            occurs_at: SystemTime::now(),
            attributes: &[],
            log_level: None,
            _private: PrivateMarker(())
        }
    }
    pub fn record(self){
        let Some(t) = globals::tracer() else { return };

        match globals::current_span() {
            Some(SpanRef(SpanRef_::Local(span))) => t.add_event(&span, self),
            span => {
                t.instrumentation_error(InstrumentationError{
                    span,
                    kind: InstrumentationErrorKind::StraySpanEvent(self)
                })
            }
        }
    }
}

// ++++++++++++++++++++ Tracer ++++++++++++++++++++

pub enum InstrumentationErrorKind<'a> {
    StraySpanAttributes(AttributeListRef<'a>),
    StraySpanStatus(SpanStatus),
    StraySpanEvent(EventArgs<'a>),
}
pub struct InstrumentationError<'a> {
    pub span: Option<SpanRef>,
    pub kind: InstrumentationErrorKind<'a>
}

pub trait Tracer: Send + Sync + 'static {
    fn open_span(&self, args: SpanArgs, _private: PrivateMarker) -> LocalSpanRef;
    fn set_attributes(&self, span: &LocalSpanRef, attrs: AttributeListRef);
    fn add_event(&self, span: &LocalSpanRef, args: EventArgs);
    fn close_span(&self, span: &LocalSpanRef, closed_at: SystemTime);
    fn drop_span(&self, idx: SpanCollectionIndex, dropped_at: SystemTime, _private: PrivateMarker);
    fn flush(&self);

    fn instrumentation_error(&self, err: InstrumentationError);
}

pub struct SpanGuard(Option<SpanRef>);

impl Drop for SpanGuard {
    fn drop(&mut self) {
        if let Some(SpanRef(SpanRef_::Local(span))) = self.0.take() {
            if let Some(f) = globals::tracer() {
                f.drop_span(span.collect_idx, SystemTime::now(), PrivateMarker(()));
            }
        }
    }
}

pub mod globals {
    use super::{SpanGuard, SpanRef, Tracer};
    use tokio::task::futures::TaskLocalFuture;
    use std::future::Future;

    static TRACER: std::sync::OnceLock<Box<dyn Tracer>> = std::sync::OnceLock::new();

    tokio::task_local! {
        static CURRENT_SPAN: SpanGuard;
    }

    pub fn set_tracer(tracer: Box<dyn Tracer>){
        TRACER.set(tracer).ok()
            .expect("tracelite: tracer already set");
    }

    pub(crate) fn tracer() -> Option<&'static dyn Tracer> {
        TRACER.get().map(|f| &**f)
    }

    pub fn current_span() -> Option<SpanRef> {
        CURRENT_SPAN.try_with(|dropper| dropper.0).ok().flatten()
    }

    pub trait InSpan: Future + Sized {
        fn in_span(self, span: SpanGuard) -> TaskLocalFuture<SpanGuard, Self> {
            CURRENT_SPAN.scope(span, self)
        }
    }

    pub fn in_span<R>(span: SpanGuard, f: impl FnOnce() -> R) -> R {
        CURRENT_SPAN.sync_scope(span, f)
    }

    // pub fn set_attributes()
}


// TODO compare with code in export.rs
// // pub trait Reporter: Send + 'static {
// //     type Span: Span;
// //     fn add_span(&mut self, span: Self::Span) -> impl Future<Output = ()> + Send;
// //     fn flush(&mut self) -> impl Future<Output = ()> + Send;
// // }

// // pub async fn reporter_run_loop<B: Reporter>(
// //     mut span_rx: UnboundedReceiver<B::Span>,
// //     mut reporter: B,
// //     min_flush_interval: Duration,
// // ){
// //     loop {
// //         match tokio::time::timeout(min_flush_interval, span_rx.recv()).await {
// //             Ok(recv) => match recv {
// //                 Some(span) => reporter.add_span(span).await,
// //                 None => {
// //                     // channel was closed
// //                     reporter.flush().await;
// //                     break
// //                 }
// //             }
// //             Err(_elapsed) => reporter.flush().await,
// //         }
// //     }
// // }

// // ++++++++++++++++++++ Recorder ++++++++++++++++++++

