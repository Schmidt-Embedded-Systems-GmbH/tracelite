use std::borrow::Cow;
use std::collections::BTreeMap;
use std::num::NonZeroU32;
use std::sync::Arc;
use std::num::{NonZeroU128, NonZeroU64};
use crate::sampling::SamplingResult;
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

impl<'a> Text<'a> {
    pub fn as_str(&self) -> Option<&'a str> {
        match self {
            Text::Static(s) => Some(s),
            Text::Borrowed(s) => Some(s),
            Text::FormatArgs(_) => None,
        }
    }
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

#[derive(Default, Debug, Clone, Copy)]
pub struct ScopedSeverityFilter {
    pub recording: Option<Severity>,
    pub sampling: Option<Severity>,
}

impl ScopedSeverityFilter {
    pub fn or(self, other: Self) -> Self {
        Self{
            recording: self.recording.or(other.recording),
            sampling: self.sampling.or(other.sampling),
        }
    }
}

#[derive(Default, Clone)]
pub struct TracingContext {
    pub baggage: BaggageMap,
    pub on_start: Option<Arc<dyn Fn(&SpanRef) + Send + Sync>>,
    pub on_ending: Option<Arc<dyn Fn(&SpanRef) + Send + Sync>>,
}

#[derive(Debug, Clone, Copy)]
pub struct RecordingSpanRef {
    pub span_context: SpanContext,
    pub collect_idx: SpanCollectionIndex,
}

#[derive(Clone)]
pub struct SpanRef {
    span_context: Option<SpanContext>,
    collect_idx: Option<SpanCollectionIndex>,
    references_ancestor: bool,
    scoped_severity_filter: ScopedSeverityFilter,
    tracing_context: Option<Arc<TracingContext>>,
}

impl SpanRef {
    pub fn disabled() -> Self {
        Self{ span_context: None, collect_idx: None, references_ancestor: false, scoped_severity_filter: Default::default(), tracing_context: None }
    }
    pub fn remote(span_context: SpanContext) -> Self {
        Self{ span_context: Some(span_context), ..Self::disabled() }
    }
    #[doc(hidden)]
    pub fn recording(rec: RecordingSpanRef) -> Self {
        Self{ collect_idx: Some(rec.collect_idx), ..Self::remote(rec.span_context) }
    }
    // #[doc(hidden)]
    // pub fn recording_ancestor(span_context: SpanContext, collect_idx: SpanCollectionIndex) -> Self {
    //     Self{ references_ancestor: true, ..Self::recording(span_context, collect_idx) }
    // }

    pub fn as_recording(&self) -> Option<RecordingSpanRef> {
        Some(RecordingSpanRef{ span_context: self.span_context?, collect_idx: self.collect_idx? })
    }
    pub fn as_recording_current(&self) -> Option<RecordingSpanRef> {
        (!self.references_ancestor).then(|| self.as_recording()).flatten()
    }

    pub fn is_recording(&self) -> bool {
        self.collect_idx.is_some()
    }
    pub fn is_recording_current(&self) -> bool {
        self.collect_idx.is_some() && !self.references_ancestor
    }
    pub fn span_context(&self) -> Option<SpanContext> {
        self.span_context
    }
    pub fn collection_index(&self) -> Option<SpanCollectionIndex> {
        self.collect_idx
    }
    pub fn tracing_context(&self) -> Option<&Arc<TracingContext>> {
        self.tracing_context.as_ref()
    }

    //TODO find a better name for this
    pub fn with_scoped_severity_filter(self, f: ScopedSeverityFilter) -> Self {
        Self{ scoped_severity_filter: f.or(self.scoped_severity_filter), ..self }
    }

    //TODO find a better name for this
    pub fn overwrite_tracing_context(self, t: Option<Arc<TracingContext>>) -> Self {
        Self{ tracing_context: t, ..self }
    }
}

// ++++++++++++++++++++ spans ++++++++++++++++++++

// todo move to attribute.rs
#[derive(Debug, Clone, Copy)]
pub struct AttributeList<'a>(pub &'a [(Text<'a>, AttributeValue<'a>)]);

impl<'a> AttributeList<'a> {
    pub fn get(&self, key: &str) -> Option<AttributeValue<'a>> {
        self.0.iter()
            .find(|(k, _)| k.as_str() == Some(key))
            .map(|(_, v)| *v)
    }
}

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
#[derive(Debug)]
pub struct SpanArgs<'a> {
    pub name: Text<'a>, 
    pub target: Option<&'static str>,
    pub severity: Option<Severity>,

    pub parent: Option<SpanContext>,
    pub status: Option<SpanStatus<'a>>,
    pub kind: Option<SpanKind>,

    pub attributes: AttributeList<'a>,

    _private: PrivateMarker,
}


pub struct SpanBuilder<'a> {
    args: SpanArgs<'a>,
    parent: Option<Option<&'a SpanRef>>,
    on_start: Option<Arc<dyn Fn(&SpanRef) + Send + Sync>>,
    on_ending: Option<Arc<dyn Fn(&SpanRef) + Send + Sync>>,
    baggage_entries: Vec<(BaggageKey, BaggageValue)>,
}

impl<'a> SpanBuilder<'a> {
    // NOTE severity is not a setable field because as tokio tracing discussions revealed, having 
    //      a default severity (log level) is a bad idea - some assume the default to be INFO, others TRACE
    pub fn new(name: impl Into<Text<'a>>, target: Option<&'static str>, severity: Option<Severity>) -> Self {
        let name = name.into();
        Self{
            args: SpanArgs{
                name,
                target,
                severity,
                parent: None,
                kind: None,
                status: None,
                attributes: AttributeList(&[]),
                _private: PrivateMarker(()),
            },
            parent: None,
            on_start: None,
            on_ending: None,
            baggage_entries: vec![],
        }
    }

    fn _start<'b>(mut self, tracer: &dyn Tracer, parent: Option<&SpanRef>, attributes: AttributeList<'b>) -> OwnedSpanRef {
        if let Some(parent) = parent {
            if parent.scoped_severity_filter.recording.is_none()
            || self.args.severity.is_none()
            || self.args.severity < parent.scoped_severity_filter.recording
            {
                if !parent.is_recording() {
                    let return_span = SpanRef{ references_ancestor: true, ..parent.clone() };
                    return OwnedSpanRef(return_span)
                }
            }

            if parent.collect_idx.is_none() {
                // TODO child span with escalating severity?
            }

            // set SpanArgs::parent
            self.args.parent = parent.span_context();
        }

        // set SpanArgs::status if unset and severity>=Error
        if self.args.status.is_none() && self.args.severity >= Some(Severity::Error) {
            self.args.status = Some(SpanStatus::error(self.args.name))
        }

        // create new SpanRef
        let mut span_ref = match tracer.start_span(SpanArgs{ attributes, ..self.args }, &mut PrivateMarker(())) {
            Some((recording_ref, sampling)) => {
                let filter = ScopedSeverityFilter{ recording: sampling.min_recording_severity, sampling: sampling.min_sampling_severity };
                SpanRef::recording(recording_ref).with_scoped_severity_filter(filter)
            }
            None => match parent {
                Some(p) => SpanRef{ references_ancestor: true, ..p.clone() },
                None => SpanRef::disabled()
            }
        };

        // build new TracingContext, if necessary
        if self.on_start.is_some() || self.on_ending.is_some() || !self.baggage_entries.is_empty() {
            let mut tracing_ctx = span_ref.tracing_context.as_ref().map(|t| (**t).clone()).unwrap_or_default();
            tracing_ctx.on_start = self.on_start.or(tracing_ctx.on_start);
            tracing_ctx.on_ending = self.on_ending.or(tracing_ctx.on_ending);
            tracing_ctx.baggage.extend(self.baggage_entries);
            span_ref.tracing_context = Some(Arc::new(tracing_ctx));
        }
        OwnedSpanRef(span_ref)
    }

    pub fn start<'b>(mut self, tracer: &dyn Tracer, attributes: AttributeList<'b>) -> OwnedSpanRef {
        match self.parent.take() {
            Some(parent) => self._start(tracer, parent, attributes),
            None => {
                let mut this = Some(self);
                globals::current_span(|parent| this.take().unwrap()._start(tracer, Some(parent), attributes))
                    .unwrap_or_else(|_| this.take().unwrap()._start(tracer, None, attributes))
            }
        }
    }

    pub fn no_parent(self) -> Self {
        Self{ parent: Some(None), ..self }
    }
    pub fn parent<'b>(self, parent: &'b SpanRef) -> SpanBuilder<'b>
        where 'a: 'b
    {
        SpanBuilder{ parent: Some(Some(parent)), ..self }
    }
    pub fn name<'b>(self, name: impl Into<Text<'b>>) -> SpanBuilder<'b>
        where 'a: 'b
    {
        SpanBuilder{ args: SpanArgs{ name: name.into(), ..self.args }, ..self }
    }
    pub fn status<'b>(self, status: SpanStatus<'a>) -> SpanBuilder<'b>
        where 'a: 'b
    {
        SpanBuilder{ args: SpanArgs{ status: Some(status), ..self.args }, ..self }
    }
    pub fn kind(self, kind: SpanKind) -> Self {
        Self{ args: SpanArgs{ kind: Some(kind), ..self.args }, ..self }
    }

    pub fn on_start(self, f: impl Fn(&SpanRef) + Send + Sync + 'static) -> Self {
        Self{ on_start: Some(Arc::new(f)), ..self }
    }
    pub fn on_ending(self, f: impl Fn(&SpanRef) + Send + Sync + 'static) -> Self {
        Self{ on_ending: Some(Arc::new(f)), ..self }
    }
    pub fn baggage_entry(mut self, key: impl Into<BaggageKey>, value:  impl Into<BaggageValue>) -> Self {
        self.baggage_entries.push((key.into(), value.into()));
        self
    }
}

pub struct OwnedSpanRef(SpanRef);

impl std::ops::Deref for OwnedSpanRef {
    type Target = SpanRef;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl Drop for OwnedSpanRef {
    fn drop(&mut self) {
        if let Some(recording) = self.0.as_recording_current() {
            // trigger on_ending
            self.0.tracing_context.as_ref()
                .and_then(|ctx| ctx.on_ending.as_ref())
                .map(|on_ending| (on_ending)(&self.0));

            let tracer = globals::tracer().unwrap();
            tracer.drop_span(recording, &mut PrivateMarker(()));
        }
    }
}

// ++++++++++++++++++++ events ++++++++++++++++++++

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
    pub attributes: AttributeList<'a>,
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
            attributes: AttributeList(&[]),
            _private: PrivateMarker(()),
        })
    }
    
    #[doc(hidden)]
    pub fn add_to_span<'b>(self, tracer: &dyn Tracer, span: &OwnedSpanRef, attributes: AttributeList<'b>){
        if span.scoped_severity_filter.recording.is_some()
        && self.0.severity.is_some()
        && self.0.severity < span.scoped_severity_filter.recording
        {
            return // discard event
        }

        // check if event can be escalated event to ancestor span
        if span.collect_idx.is_none() {
            let err = InstrumentationError::FailedEventEscalation(span, self.0);
            tracer.instrumentation_error(err);
            return
        }

        let recording = span.as_recording().unwrap();

        // set SpanStatus if severity >= Error
        if self.0.severity >= Some(Severity::Error) {
            tracer.set_status(recording, SpanStatus::Error(self.0.name));
        }

        // add event to span
        tracer.add_event(recording, EventArgs{ attributes, ..self.0 }, &mut PrivateMarker(()));
    }

    #[doc(hidden)]
    pub fn add_to_current_span<'b>(self, tracer: &dyn Tracer, attributes: AttributeList<'b>){
        let mut this = Some(self);
        globals::current_span(|span| {
            let this = this.take().unwrap();
            this.add_to_span(tracer, span, attributes);
        }).unwrap_or_else(|_| {
            let this = this.take().unwrap();
            tracer.instrumentation_error(InstrumentationError::StrayEvent(this.0));
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
    StrayAttributes(AttributeList<'a>),
    StrayEvent(EventArgs<'a>),
    StrayStatus(SpanStatus<'a>),
    FailedEventEscalation(&'a SpanRef, EventArgs<'a>),
}

impl<'a> std::fmt::Display for InstrumentationError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ERROR] tracelite: instrumentation error: ")?;
        match self {
            Self::StrayAttributes(attr) => write!(f, "stray attributes without target span: {:?}", attr),
            Self::StrayEvent(event_args) => write!(f, "stray event without target span: {:?}", event_args),
            Self::StrayStatus(span_status) => write!(f, "stray span status without target span: {:?}", span_status),
            // TODO rework text to make it more understandable
            Self::FailedEventEscalation(_span, event_args) => write!(f, "no root span recorded for event with escalating severity: {:?}", event_args),
        }
    }
}

pub trait Tracer: Send + Sync + 'static {
    fn is_location_enabled(&self, target: Option<&'static str>, severity: Option<Severity>) -> bool;

    fn start_span(&self, args: SpanArgs, _: &mut PrivateMarker) -> Option<(RecordingSpanRef, SamplingResult)>;
    fn set_attributes(&self, span: RecordingSpanRef, attrs: AttributeList);
    fn add_event(&self, span: RecordingSpanRef, args: EventArgs, _: &mut PrivateMarker);
    fn set_status(&self, span: RecordingSpanRef, status: SpanStatus);
    fn drop_span(&self, span: RecordingSpanRef, _: &mut PrivateMarker);
    fn flush(&self);

    fn instrumentation_error(&self, err: InstrumentationError);
}

impl OwnedSpanRef {
    pub fn set_attributes<'a>(&self, tracer: &dyn Tracer, attrs: AttributeList<'a>){
        if attrs.0.is_empty() { return }
        if let Some(recording) = self.as_recording_current() {
            tracer.set_attributes(recording, attrs);
        }
    }

    pub fn set_status(&self, tracer: &dyn Tracer, status: SpanStatus){
        if let Some(recording) = self.as_recording_current() {
            tracer.set_status(recording, status);
        }
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
    pub fn set_attributes<'a>(attrs: AttributeList<'a>) {
        if attrs.0.is_empty() { return }
        let Some(tracer) = tracer().ok() else { return };
        let mut attrs = Some(attrs);

        current_span(|span| {
            span.set_attributes(tracer, attrs.take().unwrap());
        }).ok().unwrap_or_else(|| {
            let attrs = attrs.take().unwrap();
            let err = InstrumentationError::StrayAttributes(attrs);
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
            if !tracer.is_location_enabled(None, severity) { return self; }

            if let Some(ex) = self.get_exception() {
                EventBuilder::new(name.into(), None, severity)
                    .exception(ex)
                    .add_to_current_span(tracer, AttributeList(&[]));
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
                if !tracer.is_location_enabled(None, severity) { return self; }

                EventBuilder::new(name.into(), None, severity)
                    .exception_dbgfmt(ex)
                    .add_to_current_span(tracer, AttributeList(&[]));
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