use std::borrow::Cow;
use std::collections::BTreeMap;
use std::num::NonZeroU32;
use std::sync::Arc;
use std::num::{NonZeroU128, NonZeroU64};
use crate::{AttributeValue, Severity};

#[derive(Debug, Clone, Copy)]
pub struct NoTracerRegistered;

impl std::fmt::Display for NoTracerRegistered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("operation requires tracer, but no tracer was registered")
    }
}

impl std::error::Error for NoTracerRegistered {}

#[derive(Debug, Clone, Copy)]
pub struct NoCurrentSpan;

impl std::fmt::Display for NoCurrentSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("operation requires span, but no span has been opened in this context")
    }
}

impl std::error::Error for NoCurrentSpan {}

// #[derive(Debug, Clone, Copy)]
// pub struct NonRecordingRootSpan;

// impl std::fmt::Display for NonRecordingRootSpan {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_str("operation requires root span to be recording, but it was not sampled for recording")
//     }
// }

// impl std::error::Error for NonRecordingRootSpan {}

/// Can only be constructed by this crate - used to mark operation which
/// must not be freely called by users, but only in one specific circumstance.
#[derive(Debug)]
pub struct PrivateMarker(());

// ++++++++++++++++++++ Text ++++++++++++++++++++

/// TODO with the addition of FormatArgs, the name does not fit anymore
#[derive(Debug, Clone, Copy)]
pub enum Text<'a> {
    Static(&'static str),
    Borrowed(&'a str),
    FormatArgs(std::fmt::Arguments<'a>)
}

impl From<&'static str> for Text<'static> {
    fn from(s: &'static str) -> Self { Self::Static(s) }
}

impl<'a> From<std::fmt::Arguments<'a>> for Text<'a> {
    fn from(a: std::fmt::Arguments<'a>) -> Self { Self::FormatArgs(a) }
}

impl<'a> std::fmt::Display for Text<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Text::Static(x) => x.fmt(f),
            Text::Borrowed(x) => x.fmt(f),
            Text::FormatArgs(x) => x.fmt(f),
        }
    }
}

// ++++++++++++++++++++ baggage ++++++++++++++++++++

pub type BaggageKey = Cow<'static, str>;
pub type BaggageValue = Cow<'static, str>;
pub type BaggageMap = BTreeMap<BaggageKey, BaggageValue>;

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
        NonZeroU128::new(int).map(Self).ok_or("trace-id string has no non-zero bytes")
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
        NonZeroU64::new(int).map(Self).ok_or("span-id string has no non-zero bytes")
    }
}

// ++++++++++++++++++++ SpanContext ++++++++++++++++++++

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanContext {
    pub trace_id: TraceId,
    pub span_id: SpanId,
    pub trace_flags: u8,
}

impl SpanContext {
    pub fn sampled(&self) -> bool {
        self.trace_flags & 0 != 0
    }
}

impl std::fmt::Display for SpanContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "00-{}-{}-{:02x}", self.trace_id, self.span_id, self.trace_flags)
    }
}

impl std::str::FromStr for SpanContext {
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

// ++++++++++++++++++++ SpanParent ++++++++++++++++++++

// TODO is NonZero worth it?
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanCollectionIndex(pub NonZeroU32, pub u32);

#[derive(Default, Clone)]
pub struct TracingContext {
    pub baggage: BaggageMap,
    pub on_start: Option<Arc<dyn Fn(&RecordingSpanRef) + Send + Sync>>,
    pub on_ending: Option<Arc<dyn Fn(&RecordingSpanRef) + Send + Sync>>,
}

#[derive(Clone)]
pub struct RecordingSpanRef {
    pub span_context: SpanContext,
    pub collect_idx: SpanCollectionIndex,
    pub tracing_context: Option<Arc<TracingContext>>,
}

// TODO can we further reduce the size of this thing?
#[derive(Clone)]
pub struct LocalSpanRef {
    // if None, is_recording MUST be false
    // TODO make this non-pub
    pub recording: Option<RecordingSpanRef>,
    is_recording: bool,
    min_recording_level: Option<Severity>,
    min_sampling_level: Option<Severity>,
}

impl LocalSpanRef {
    #[doc(hidden)]
    pub fn non_recording(
        ancestor: Option<RecordingSpanRef>,
        min_recording_level: Option<Severity>,
        min_sampling_level: Option<Severity>,
    ) -> Self {
        Self{ recording: ancestor, is_recording: false, min_recording_level, min_sampling_level }
    }
    #[doc(hidden)]
    pub fn recording(
        recording: Option<RecordingSpanRef>,
        min_recording_level: Option<Severity>,
        min_sampling_level: Option<Severity>,
    ) -> Self {
        Self{ recording, is_recording: true, min_recording_level, min_sampling_level }
    }

    pub fn as_parent(&self) -> SpanParent {
        SpanParent::Local(self.clone())
    }

    /// if span is not recording, this will still return a recording ancestor
    pub fn span_context(&self) -> Option<SpanContext> {
        Some(self.recording.as_ref()?.span_context)
    }

    pub fn is_recording(&self) -> bool {
        self.is_recording
    }
    /// if span is not recording, this will still return the trace-id of a recording ancestor
    pub fn trace_id(&self) -> Option<TraceId> {
        Some(self.span_context()?.trace_id)
    }
    /// if span is not recording, this will return None
    pub fn span_id(&self) -> Option<SpanId> {
        if !self.is_recording { return None }
        Some(self.span_context().unwrap().span_id)
    }

    pub fn baggage(&self) -> Option<&BaggageMap> {
        Some(&self.recording.as_ref()?.tracing_context.as_ref()?.baggage)
    }
}

#[derive(Clone)]
pub enum SpanParent {
    Local(LocalSpanRef),
    Remote(SpanContext),
}

impl Into<Option<SpanParent>> for LocalSpanRef {
    fn into(self) -> Option<SpanParent> {
        Some(SpanParent::Local(self))
    }
}

impl<'a> Into<Option<SpanParent>> for &'a OwnedSpanRef {
    fn into(self) -> Option<SpanParent> {
        Some(SpanParent::Local(self.0.clone()))
    }
}

// ++++++++++++++++++++ span/event args ++++++++++++++++++++

pub type AttributeListRef<'a> = &'a [(Text<'a>, AttributeValue<'a>)];
pub type AttributeList<'a, const N: usize> = [(Text<'a>, AttributeValue<'a>); N];

#[derive(Debug, Clone)]
pub enum SpanStatus<'a> {
    Ok,
    Error(Text<'a>),
}

impl<'a> SpanStatus<'a> {
    pub fn error(s: impl Into<Text<'a>>) -> Self {
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
    // TODO hode fields
    pub name: Text<'a>, 
    pub target: Option<&'static str>,
    pub severity: Option<Severity>,
    // TODO make use of this
    pub severity_sampling_recommendation: Option<bool>,

    // NOTE this will NOT be available to SpanCOllection
    pub parent: Option<Option<SpanParent>>,
    // NOTE this will only be available to SpanCollection
    pub parent_span_id: Option<SpanId>,

    pub status: Option<SpanStatus<'a>>,
    pub kind: Option<SpanKind>,

    pub attributes: AttributeListRef<'a>,

    pub on_start: Option<Arc<dyn Fn(&RecordingSpanRef) + Send + Sync>>,
    pub on_ending: Option<Arc<dyn Fn(&RecordingSpanRef) + Send + Sync>>,
    pub baggage_entries: Vec<(BaggageKey, BaggageValue)>,

    _private: PrivateMarker,
}

pub struct SpanBuilder<'a>(SpanArgs<'a>);

impl<'a> SpanBuilder<'a> {
    // NOTE severity is not a setable field because as tokio tracing discussions revealed, having 
    //      a default severity (log level) is a bad idea - some assume the default to be INFO, others TRACE
    pub fn new(name: impl Into<Text<'a>>, target: Option<&'static str>, severity: Option<Severity>) -> Self {
        let name = name.into();
        Self(SpanArgs{
            name,
            target,
            severity,
            severity_sampling_recommendation: None,
            parent: None,
            parent_span_id: None,
            kind: None,
            status: None,
            attributes: &[],
            on_start: None,
            on_ending: None,
            baggage_entries: vec![],
            _private: PrivateMarker(()),
        })
    }

    pub fn start<'b, const N: usize>(
        mut self,
        tracer: &dyn Tracer,
        attrs: AttributeList<'b, N>,
    ) -> OwnedSpanRef {
        if self.0.parent.is_none() {
            self.0.parent = Some(globals::current_span(|span| (*span).as_parent()).ok());
        }
        if self.0.severity >= Some(Severity::Error) && self.0.status.is_none() {
            self.0.status = Some(SpanStatus::error(self.0.name))
        }

        if let Some(SpanParent::Local(parent)) = self.0.parent.as_ref().unwrap() {
            let min_recording_level = parent.min_recording_level;
            let min_sampling_level = parent.min_sampling_level;

            if self.0.severity.is_some() && self.0.severity < min_recording_level {
                let non_recording = LocalSpanRef::non_recording(
                    parent.recording.clone(),
                    min_recording_level,
                    min_sampling_level,
                );
                return OwnedSpanRef(non_recording)
            }

            let mut span_ref = tracer.start_span(SpanArgs{ attributes: &attrs, ..self.0 }, &mut PrivateMarker(()));
            span_ref.min_recording_level = span_ref.min_recording_level.or(min_recording_level);
            span_ref.min_sampling_level = span_ref.min_sampling_level.or(min_sampling_level);
            OwnedSpanRef(span_ref)
        } else {
            let local = tracer.start_span(SpanArgs{ attributes: &attrs, ..self.0 }, &mut PrivateMarker(()));
            OwnedSpanRef(local)
        }
    }

    pub fn parent(self, parent: impl Into<Option<SpanParent>>) -> Self {
        Self(SpanArgs{ parent: Some(parent.into()), ..self.0 })
    }
    pub fn name<'b>(self, name: impl Into<Text<'b>>) -> SpanBuilder<'b>
        where 'a: 'b
    {
        SpanBuilder(SpanArgs{ name: name.into(), ..self.0 })
    }
    pub fn status<'b>(self, status: SpanStatus<'a>) -> SpanBuilder<'b>
        where 'a: 'b
    {
        SpanBuilder(SpanArgs{ status: Some(status), ..self.0 })
    }
    pub fn kind(self, kind: SpanKind) -> Self {
        Self(SpanArgs{ kind: Some(kind), ..self.0 })
    }

    pub fn on_start(self, f: impl Fn(&RecordingSpanRef) + Send + Sync + 'static) -> Self {
        Self(SpanArgs{ on_start: Some(Arc::new(f)), ..self.0 })
    }
    pub fn on_ending(self, f: impl Fn(&RecordingSpanRef) + Send + Sync + 'static) -> Self {
        Self(SpanArgs{ on_ending: Some(Arc::new(f)), ..self.0 })
    }
    pub fn baggage_entry(mut self, key: impl Into<BaggageKey>, value:  impl Into<BaggageValue>) -> Self {
        self.0.baggage_entries.push((key.into(), value.into()));
        Self(SpanArgs{ baggage_entries: self.0.baggage_entries, ..self.0 })
    }
}

pub struct OwnedSpanRef(LocalSpanRef);

impl std::ops::Deref for OwnedSpanRef {
    type Target = LocalSpanRef;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl Drop for OwnedSpanRef {
    fn drop(&mut self) {
        println!("DROPPED {:?}", self.0.is_recording);
        match &self.0 {
            LocalSpanRef{ recording: Some(recording), is_recording: true, .. } => {
                // span_ref.tracing_context.as_ref()
                //     .and_then(|ctx| ctx.on_ending.as_ref())
                //     .map(|on_ending| (on_ending)(span_ref));
                let tracer = globals::tracer().unwrap();
                tracer.drop_span(recording, &mut PrivateMarker(()));
            }
            _ => {}
        }
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub enum Exception<'a> {
    Error{
        object: &'a (dyn std::error::Error + 'a),
        type_name: &'static str
    },
    Dbgfmt{
        object: &'a (dyn std::fmt::Debug + 'a),
        type_name: &'static str
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub struct EventArgs<'a> {
    // NOTE is `exception` is set, this may be interpreted as the span status message
    pub name: Text<'a>,
    pub target: Option<&'a str>,
    pub severity: Option<Severity>,
    pub exception: Option<Exception<'a>>,
    pub attributes: AttributeListRef<'a>,
    _private: PrivateMarker,
}

pub struct EventBuilder<'a>(EventArgs<'a>);

impl<'a> EventBuilder<'a> {
    pub fn new(name: impl Into<Text<'a>>, target: Option<&'a str>, severity: Option<Severity>) -> Self {
        Self(EventArgs{
            name: name.into(),
            target,
            severity,
            exception: None,
            attributes: &[],
            _private: PrivateMarker(()),
        })
    }
    
    pub fn add_to_current_span<'b, const N: usize>(
        self,
        tracer: &dyn Tracer,
        attributes: AttributeList<'b, N>,
    ){
        let mut this = Some(self.0);
        globals::current_span(|span| {
            let args = this.take().unwrap();

            if args.severity.is_some() && args.severity < span.min_recording_level {
                return // not recorded
            }

            match &span.0.recording {
                Some(recording) => {
                    if args.severity >= Some(Severity::Error) {
                        tracer.set_status(recording, SpanStatus::Error(args.name));
                    }
                    tracer.add_event(recording, EventArgs{ attributes: &attributes, ..args });
                }
                None => {
                    tracer.instrumentation_error(InstrumentationError::EventSeverityEscalation(span.min_recording_level, args));
                }
            }
        }).unwrap_or_else(|_| {
            let this = this.take().unwrap();
            tracer.instrumentation_error(InstrumentationError::StrayEvent(this));
        });
    }

    pub fn exception<'b>(self, object: &'b (impl std::error::Error + 'b)) -> EventBuilder<'b>
        where 'a: 'b
    {
        let type_name = std::any::type_name_of_val(object);
        EventBuilder(EventArgs{ exception: Some(Exception::Error{ object, type_name }), ..self.0 })
    }

    pub fn exception_dbgfmt<'b>(self, object: &'b (impl std::fmt::Debug + 'b)) -> EventBuilder<'b>
        where 'a: 'b
    {
        let type_name = std::any::type_name_of_val(object);
        EventBuilder(EventArgs{ exception: Some(Exception::Dbgfmt{ object, type_name }), ..self.0 })
    }
}

// ++++++++++++++++++++ Tracer ++++++++++++++++++++

#[non_exhaustive]
pub enum InstrumentationError<'a> {
    StrayAttributes(AttributeListRef<'a>),
    StrayEvent(EventArgs<'a>),
    StrayStatus(SpanStatus<'a>),
    EventSeverityEscalation(Option<Severity>, EventArgs<'a>),
}

impl<'a> std::fmt::Display for InstrumentationError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ERROR] tracelite: instrumentation error: ")?;
        match self {
            Self::StrayAttributes(attr) => write!(f, "stray attributes without target span: {:?}", attr),
            Self::StrayEvent(event_args) => write!(f, "stray event without target span: {:?}", event_args),
            Self::StrayStatus(span_status) => write!(f, "stray span status without target span: {:?}", span_status),
            // TODO rework text to make it more understandable
            Self::EventSeverityEscalation(severity, span_status) => write!(f, "no root span recorded for event with escalating severity (>= {severity:?}): {:?}", span_status),
        }
    }
}

pub trait Tracer: Send + Sync + 'static {
    fn is_enabled(&self, target: Option<&'static str>, severity: Option<Severity>) -> bool;

    fn start_span(&self, args: SpanArgs, _: &mut PrivateMarker) -> LocalSpanRef;
    fn set_attributes(&self, span: &RecordingSpanRef, attrs: AttributeListRef);
    fn add_event(&self, span: &RecordingSpanRef, args: EventArgs);
    fn set_status(&self, span: &RecordingSpanRef, status: SpanStatus);
    fn drop_span(&self, span: &RecordingSpanRef, _: &mut PrivateMarker);
    fn flush(&self);

    fn instrumentation_error(&self, err: InstrumentationError);
}

impl OwnedSpanRef {
    pub fn set_attributes<'a, const N: usize>(&self, tracer: &dyn Tracer, attrs: AttributeList<'a, N>){
        if N == 0 { return }
        if !self.is_recording { return };
        tracer.set_attributes(self.recording.as_ref().unwrap(), &attrs);
    }

    pub fn set_status(&self, tracer: &dyn Tracer, status: SpanStatus){
        if !self.is_recording { return };
        tracer.set_status(self.recording.as_ref().unwrap(), status);
    }
}

pub mod globals {
    use super::{AttributeList, EventBuilder, InstrumentationError, NoCurrentSpan, NoTracerRegistered, OwnedSpanRef, SpanStatus, Text, Tracer};
    use crate::Severity;
    use tokio::task::futures::TaskLocalFuture;
    use std::future::Future;

    static TRACER: std::sync::OnceLock<Box<dyn Tracer>> = std::sync::OnceLock::new();

    tokio::task_local! {
        static CURRENT_SPAN: OwnedSpanRef;
    }

    pub fn set_tracer(tracer: Box<dyn Tracer>){
        TRACER.set(tracer).ok()
            .expect("[FATAL] tracelite: tracer already set");
    }

    pub fn tracer() -> Result<&'static dyn Tracer, NoTracerRegistered> {
        TRACER.get().map(|f| &**f).ok_or(NoTracerRegistered)
    }

    pub fn current_span<T>(f: impl FnOnce(&OwnedSpanRef) -> T) -> Result<T, NoCurrentSpan> {
        CURRENT_SPAN.try_with(f).map_err(|_| NoCurrentSpan)
    }

    pub enum MaybeInSpan<F>{
        InSpan(TaskLocalFuture<OwnedSpanRef, F>),
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
        fn in_span(self, span: impl Into<Option<OwnedSpanRef>>) -> MaybeInSpan<Self> {
            if let Some(span) = span.into() {
                MaybeInSpan::InSpan(CURRENT_SPAN.scope(span, self))
            } else {
                MaybeInSpan::NotInSpan(self)
            }
        }
    }

    impl<F: Future + Sized> InSpan for F {}

    pub fn sync_in_span<R>(span: impl Into<Option<OwnedSpanRef>>, f: impl FnOnce() -> R) -> R {
        if let Some(span) = span.into() {
            CURRENT_SPAN.sync_scope(span, f)
        } else {
            f()
        }
    }

    /// NOTE you will likely want to use span_attributes!() instead
    pub fn set_attributes<'a, const N: usize>(attrs: AttributeList<'a, N>) {
        if N == 0 { return }
        let Some(tracer) = tracer().ok() else { return };
        let mut attrs = Some(attrs);

        current_span(|span| {
            span.set_attributes(tracer, attrs.take().unwrap());
        }).ok().unwrap_or_else(|| {
            let attrs = attrs.take().unwrap();
            let err = InstrumentationError::StrayAttributes(&attrs);
            tracer.instrumentation_error(err);
        });
    }

    pub fn set_status(status: SpanStatus<'_>){
        let Some(tracer) = tracer().ok() else { return };
        let mut status = Some(status);

        current_span(|span| {
            span.set_status(tracer, status.take().unwrap());
        }).ok().unwrap_or_else(|| {
            let status = status.take().unwrap();
            let err = InstrumentationError::StrayStatus(status);
            tracer.instrumentation_error(err);
        });
    }

    pub fn set_ok_status(){
        set_status(SpanStatus::Ok);
    }

    pub fn set_error_status<'a>(msg: impl Into<Text<'a>>){
        set_status(SpanStatus::error(msg));
    }

    pub trait RecordException<E: std::error::Error>: Sized {
        #[doc(hidden)]
        fn get_exception(&self) -> Option<&E>;

        #[doc(hidden)]
        fn _record_error_ext<'a>(self,
            name: impl Into<Text<'a>>,
            severity: Option<Severity>,
        ) -> Self {
            let Some(tracer) = tracer().ok() else { return self };
            if !tracer.is_enabled(None, severity) { return self; }

            if let Some(ex) = self.get_exception() {
                EventBuilder::new(name.into(), None, severity)
                    .exception(ex)
                    .add_to_current_span(tracer, []);
            }
            self
        }

        fn record_error_as_debug<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Debug))
        }
        fn record_error_as_info<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Info))
        }
        fn record_error_as_warn<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Warn))
        }
        fn record_error<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Error))
        }
        fn record_error_as_fatal<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Fatal))
        }

        fn record_exception(self) -> Self {
            self._record_error_ext("exception", None)
        }
        fn record_exception_as_trace(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Trace))
        }
        fn record_exception_as_debug(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Debug))
        }
        fn record_exception_as_info(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Info))
        }
        fn record_exception_as_warn(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Warn))
        }
    }

    impl<E: std::error::Error> RecordException<E> for Option<E> {
        fn get_exception(&self) -> Option<&E> { self.as_ref() }
    }

    impl<T, E: std::error::Error> RecordException<E> for Result<T, E> {
        fn get_exception(&self) -> Option<&E> { self.as_ref().err() }
    }

    pub trait RecordExceptionDebugFmt<E: std::fmt::Debug>: Sized {
        #[doc(hidden)]
        fn get_exception(&self) -> Option<&E>;

        #[doc(hidden)]
        fn _record_error_ext<'a>(self,
            name: impl Into<Text<'a>>,
            severity: Option<Severity>,
        ) -> Self {
            if let Some(ex) = self.get_exception() {
                let Some(tracer) = tracer().ok() else { return self };
                if !tracer.is_enabled(None, severity) { return self; }

                EventBuilder::new(name.into(), None, severity)
                    .exception_dbgfmt(ex)
                    .add_to_current_span(tracer, []);
            }
            self
        }

        fn record_dbgfmt_error_as_debug<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Debug))
        }
        fn record_dbgfmt_error_as_info<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Info))
        }
        fn record_dbgfmt_error_as_warn<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Warn))
        }
        fn record_dbgfmt_error<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Error))
        }
        fn record_dbgfmt_error_as_fatal<'a>(self, name: impl Into<Text<'a>>) -> Self {
            self._record_error_ext(name, Some(Severity::Fatal))
        }

        fn record_dbgfmt_exception(self) -> Self {
            self._record_error_ext("exception", None)
        }
        fn record_dbgfmt_exception_as_trace(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Trace))
        }
        fn record_dbgfmt_exception_as_debug(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Debug))
        }
        fn record_dbgfmt_exception_as_info(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Info))
        }
        fn record_dbgfmt_exception_as_warn(self) -> Self {
            self._record_error_ext("exception", Some(Severity::Warn))
        }
    }

    impl<E: std::fmt::Debug> RecordExceptionDebugFmt<E> for Option<E> {
        fn get_exception(&self) -> Option<&E> { self.as_ref() }
    }

    impl<T, E: std::fmt::Debug> RecordExceptionDebugFmt<E> for Result<T, E> {
        fn get_exception(&self) -> Option<&E> { self.as_ref().err() }
    }
}