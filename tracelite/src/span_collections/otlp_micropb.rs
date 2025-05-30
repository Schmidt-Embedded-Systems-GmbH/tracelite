use crate::tracer::{AttributeList, EventArgs, Text, SpanArgs, SpanCollectionIndex, SpanId, SpanKind, SpanStatus, TraceId};
use crate::{AttributeValue, Exception};
use opentelemetry_micropb::std::collector_::trace_::v1_::{self as collector, ExportTraceServiceRequest};
use opentelemetry_micropb::std::common_::v1_ as common;
use opentelemetry_micropb::std::trace_::v1_ as trace;
use opentelemetry_micropb::std::resource_::v1_ as resource;
use std::num::NonZeroU32;
use std::time::SystemTime;

#[non_exhaustive]
pub struct SpanCollectionConfig {
    resource: resource::Resource,
    autoflush_batch_size: Option<u32>,
    // TODO make use of this
    // max_events_per_span: Option<u32>,
    // TODO make use of this
    // max_attributes_per_span: Option<u32>,
}

impl super::SpanCollectionConfig for SpanCollectionConfig {
    type SpanCollection = SpanCollection;

    fn new(service_name: &str, resource_attrs: AttributeList) -> Self {
        let mut resource = resource::Resource::default();

        resource.attributes = [("service.name".into(), AttributeValue::from(service_name))].iter()
            .chain(resource_attrs.0)
            .flat_map(map_kv)
            .collect();

        Self {
            resource,
            autoflush_batch_size: None,
            // max_events_per_span: None,
            // max_attributes_per_span: None
        }
    }

    fn build(self) -> SpanCollection {
        let mut resource_span = trace::ResourceSpans::default();
        resource_span.set_resource(self.resource.clone()); // TODO build resource in this method
        resource_span.scope_spans = vec![Default::default()];
        SpanCollection{
            config: self,
            spans: vec![],
            free_indicies: vec![],
            exportable: collector::ExportTraceServiceRequest{ resource_spans: vec![resource_span] },
        }
    }

    // TODO
    // pub fn autoflush_batch_size(self, s: u32) -> Self {
    //     Self{ autoflush_batch_size: Some(s), ..self }
    // }
}

pub struct SpanCollection {
    config: SpanCollectionConfig,
    spans: Vec<Option<trace::Span>>,
    free_indicies: Vec<u32>,
    exportable: ExportTraceServiceRequest,
}

impl SpanCollection {
    fn get_open_span_mut(&mut self, idx: SpanCollectionIndex) -> Option<&mut trace::Span> {
        let span = self.spans.get_mut(idx.1 as usize)?.as_mut()?;
        if span.end_time_unix_nano != 0 { return None }
        Some(span)
    }
}

impl super::SpanCollection for SpanCollection {
    type Exportable = ExportTraceServiceRequest;

    fn open_span(&mut self,
        trace_id: TraceId,
        span_id: SpanId,
        args: SpanArgs,
        opened_at: u64,
    ) -> Result<SpanCollectionIndex, ()> {
        let idx = match self.free_indicies.pop() {
            Some(idx) => idx,
            None => {
                self.spans.push(None);
                self.spans.len() as u32 - 1
            }
        };

        let mut pb_span = trace::Span::default();

        // TODO should we use little-endian bytes?
        pb_span.trace_id = trace_id.0.get().to_be_bytes().into_iter().collect();
        pb_span.span_id = span_id.0.get().to_be_bytes().into_iter().collect();
        pb_span.parent_span_id = args.parent.map(|p| p.span_id.0.get().to_be_bytes().into_iter().collect()).unwrap_or_default();
        pb_span.name = args.name.to_string();
        if let Some(kind) = args.kind {
            pb_span.kind = match kind {
                SpanKind::Internal => trace::Span_::SpanKind::Internal,
                SpanKind::Client => trace::Span_::SpanKind::Client,
                SpanKind::Server => trace::Span_::SpanKind::Server,
                SpanKind::Producer => trace::Span_::SpanKind::Producer,
                SpanKind::Consumer => trace::Span_::SpanKind::Consumer,
            };
        } else {
            pb_span.kind = trace::Span_::SpanKind::Unspecified;
        }
        pb_span.start_time_unix_nano = opened_at;
        pb_span.attributes = args.attributes.0.iter().flat_map(map_kv).collect();
        if let Some(status) = args.status {
            pb_span.set_status(map_span_status(status));
        }

        debug_assert!(self.spans[idx as usize].is_none());
        self.spans[idx as usize] = Some(pb_span);

        Ok(SpanCollectionIndex(NonZeroU32::MIN, idx))
    }

    fn set_attributes(&mut self, idx: SpanCollectionIndex, attrs: AttributeList) -> Result<(), ()> {
        let Some(span) = self.get_open_span_mut(idx) else { return Ok(()) };
        span.attributes.extend(attrs.0.iter().flat_map(map_kv));
        Ok(())
    }

    fn set_status(&mut self, idx: SpanCollectionIndex, status: SpanStatus) {
        let Some(span) = self.get_open_span_mut(idx) else { return };
        span.set_status(map_span_status(status));
    }

    fn add_event(&mut self, idx: SpanCollectionIndex, event: EventArgs, occurs_at: u64) -> Result<(), ()> {
        let Some(span) = self.get_open_span_mut(idx) else { return Ok(()) };
        let mut pb_event = trace::Span_::Event::default();

        pb_event.time_unix_nano = occurs_at;
        pb_event.name = event.name.to_string();
        match event.exception {
            Some(Exception::Error{ object, type_name }) => {
                // TODO add exception.source_chain for Error::source()?
                // TODO some support for backtrace? https://doc.rust-lang.org/std/error/struct.Request.html
                pb_event.attributes.extend([
                    (Text::from("exception.message"), AttributeValue::DynDisplay(&object)),
                    (Text::from("exception.type"), AttributeValue::from(type_name)),
                ].iter().flat_map(map_kv))
            }
            Some(Exception::Dbgfmt{ object, type_name }) => {
                pb_event.attributes.extend([
                    (Text::from("exception.message"), AttributeValue::DynDebug(&object)),
                    (Text::from("exception.type"), AttributeValue::from(type_name)),
                ].iter().flat_map(map_kv))
            }
            None => {}
        }
        pb_event.attributes.extend(event.attributes.0.iter().flat_map(map_kv));

        span.events.push(pb_event);
        Ok(())
    }

    fn close_span(&mut self, idx: SpanCollectionIndex, closed_at: SystemTime) {
        let Some(span) = self.get_open_span_mut(idx) else { return };
        span.end_time_unix_nano = closed_at.duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos() as u64;
    }

    fn drop_span(&mut self,
        idx: SpanCollectionIndex,
        dropped_at: u64,
        export: impl Fn(Self::Exportable),
    ) {
        let Some(mut span) = self.spans[idx.1 as usize].take() else {
            eprintln!("[ERROR] tracelite: dropped span which does not exist");
            return // no span here TODO should log error
        };
        self.free_indicies.push(idx.1);

        if span.end_time_unix_nano == 0 {
            span.end_time_unix_nano = dropped_at;
        }

        let export_spans_scope = &mut self.exportable.resource_spans[0].scope_spans[0];
        export_spans_scope.spans.push(span);
        if let Some(size) = self.config.autoflush_batch_size {
            if export_spans_scope.spans.len() > size as usize {
                self.flush(export);
            }
        }
    }
    
    fn flush(&mut self, export: impl Fn(Self::Exportable)) {
        self.spans.retain_mut(|entry| {
            // println!("ENTRY {entry:?}");
            if entry.is_none() { return true }
            if entry.as_mut().unwrap().end_time_unix_nano == 0 { return true }

            let export_spans_scope = &mut self.exportable.resource_spans[0].scope_spans[0];
            export_spans_scope.spans.push(entry.take().unwrap());
            true
        });

        if self.exportable.resource_spans[0].scope_spans[0].spans.is_empty() {
            println!("[INFO] tracelite: nothing to export");
            return // no spans to export
        }

        export(self.exportable.clone());
        self.exportable.resource_spans[0].scope_spans[0].spans.clear();
    }
}

fn map_value(v: &AttributeValue) -> Option<common::AnyValue> {
    let mut char_buf = [0u8; std::mem::size_of::<char>()];

    let pb_v = match v {
        AttributeValue::NotPresent => return None,

        AttributeValue::Unit     => common::AnyValue_::Value::StringValue("()".to_owned()),

        AttributeValue::Bool(x)  => common::AnyValue_::Value::BoolValue(*x),
        AttributeValue::Char(x)  => common::AnyValue_::Value::StringValue(x.encode_utf8(&mut char_buf).to_owned()),
        AttributeValue::U64(x)   => common::AnyValue_::Value::IntValue(*x as i64), // NOTE unconditional conversion, x>i64::MAX will overflow into negative
        AttributeValue::I64(x)   => common::AnyValue_::Value::IntValue(*x),
        AttributeValue::F64(x)   => common::AnyValue_::Value::DoubleValue(*x),
        AttributeValue::Str(x)   => common::AnyValue_::Value::StringValue((*x).to_owned()),
        AttributeValue::Bytes(x) => common::AnyValue_::Value::BytesValue((*x).to_owned()),

        AttributeValue::DynDisplay(x)   => common::AnyValue_::Value::StringValue(x.to_string()),
        AttributeValue::DynDebug(x)     => common::AnyValue_::Value::StringValue(format!("{x:?}")),
        #[cfg(feature = "serde")]
        AttributeValue::DynSerialize(x) => common::AnyValue_::Value::StringValue(serde_json::to_string_pretty(x).ok()?), // NOTE serde_json is just a temporary solution
    };
    Some(common::AnyValue{ value: Some(pb_v) })
}

fn map_kv(kv: &(Text, AttributeValue)) -> Option<common::KeyValue> {
    let mut pb_kv = common::KeyValue::default();
    pb_kv.key = kv.0.to_string();
    pb_kv.set_value(map_value(&kv.1)?);
    Some(pb_kv)
}

fn map_span_status(status: SpanStatus) -> trace::Status {
    match status {
        SpanStatus::Ok => {
            trace::Status{ code: trace::Status_::StatusCode::Ok, message: "".to_owned() }
        }
        SpanStatus::Error(msg) => {
            trace::Status{ code: trace::Status_::StatusCode::Error, message: msg.to_string() }
        }
    }
}