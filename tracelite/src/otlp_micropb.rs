use crate::default_tracer::SpanCollection;
use crate::internal::{AttributeListRef, EventArgs, Key, PrivateMarker, SpanArgs, SpanCollectionIndex, SpanId, SpanKind, SpanStatus, TraceId};
use opentelemetry_micropb::std::common_::v1_::{self as common};
use opentelemetry_micropb::std::trace_::v1_ as trace;
use opentelemetry_micropb::std::resource_::v1_ as resource;
use micropb::MessageEncode;
use std::io::Write;
use std::time::SystemTime;

fn map_value(v: &log::kv::Value) -> common::AnyValue {
    let pb_v = if let Some(x) = v.to_bool() {
        common::AnyValue_::Value::BoolValue(x)
    } else if let Some(x) = v.to_i64() {
        common::AnyValue_::Value::IntValue(x)
    } else if let Some(x) = v.to_f64() {
        common::AnyValue_::Value::DoubleValue(x)
    } else if let Some(x) = v.to_cow_str() {
        common::AnyValue_::Value::StringValue(x.into_owned())
    } else {
        todo!();
    };
    common::AnyValue{ value: Some(pb_v) }
}

fn map_kv(kv: &(Key, log::kv::Value)) -> common::KeyValue {
    let mut pb_kv = common::KeyValue::default();
    pb_kv.key = kv.0.to_string();
    pb_kv.set_value(map_value(&kv.1));
    pb_kv
}

fn map_span_status(status: SpanStatus) -> trace::Status {
    let message = String::new(); // TODO make use of this
    let code = match status {
        SpanStatus::Ok => trace::Status_::StatusCode::Ok,
        SpanStatus::Error => trace::Status_::StatusCode::Error,
    };
    trace::Status{ code, message }
}

#[derive(Setters)]
#[setters(strip_option, prefix="")]
pub struct OtlpMicroPbConfig {
    #[setters(skip)]
    resource: resource::Resource,
    autoflush_batch_size: Option<u32>,
    max_events_per_span: Option<u32>,
    max_attributes_per_span: Option<u32>,
}

impl OtlpMicroPbConfig {
    pub fn new(service_name: &str, resource_attrs: AttributeListRef) -> Self {
        let mut resource = resource::Resource::default();

        resource.attributes = [("service.name".into(), log::kv::Value::from_any(&service_name))].iter()
            .chain(resource_attrs)
            .map(map_kv)
            .collect();

        Self {
            resource,
            autoflush_batch_size: None,
            max_events_per_span: None,
            max_attributes_per_span: None
        }
    }
}

pub struct OtlpMicropbSpanCollection {
    config: OtlpMicroPbConfig,
    spans: Vec<Option<trace::Span>>,
    free_indicies: Vec<u32>,
    dropped_spans: trace::ResourceSpans,
}

impl OtlpMicropbSpanCollection {
    fn get_open_span_mut(&mut self, idx: SpanCollectionIndex) -> Option<&mut trace::Span> {
        let span = self.spans.get_mut(idx.0 as usize)?.as_mut()?;
        if span.end_time_unix_nano == 0 { return None }
        Some(span)
    }
}

impl SpanCollection for OtlpMicropbSpanCollection {
    type Exportable = Vec<u8>; // serialized ResourceSpans

    fn open_span(&mut self,
        trace_id: TraceId,
        span_id: SpanId,
        args: SpanArgs,
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
        pb_span.trace_id = trace_id.0.get().to_le_bytes().into_iter().collect();
        pb_span.span_id = span_id.0.get().to_le_bytes().into_iter().collect();
        pb_span.parent_span_id = args.parent.map(|r| r.span_id().0.get().to_le_bytes().into_iter().collect()).unwrap_or_default();
        pb_span.name = args.name.to_string();
        if let Some(kind) = args.span_kind {
            pb_span.kind = match kind {
                SpanKind::Internal => trace::Span_::SpanKind::Internal,
                SpanKind::Client => trace::Span_::SpanKind::Client,
                SpanKind::Server => trace::Span_::SpanKind::Server,
                SpanKind::Producer => trace::Span_::SpanKind::Producer,
                SpanKind::Consumer => trace::Span_::SpanKind::Consumer,
            };
        }
        pb_span.start_time_unix_nano = args.opened_at.duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos() as u64;
        pb_span.attributes = args.init_attributes.iter().map(map_kv).collect();
        if let Some(status) = args.span_status {
            pb_span.set_status(map_span_status(status));
        }

        Ok(SpanCollectionIndex(idx, 0))
    }

    fn set_attributes(&mut self, idx: SpanCollectionIndex, attrs: AttributeListRef) -> Result<(), ()> {
        let Some(span) = self.get_open_span_mut(idx) else { return Ok(()) };
        span.attributes.extend(attrs.iter().map(map_kv));
        Ok(())
    }

    fn set_status(&mut self, idx: SpanCollectionIndex, status: SpanStatus) {
        let Some(span) = self.get_open_span_mut(idx) else { return };
        span.status = map_span_status(status);
    }

    fn add_event(&mut self, idx: SpanCollectionIndex, event: EventArgs) -> Result<(), ()> {
        let Some(span) = self.get_open_span_mut(idx) else { return Ok(()) };
        let mut pb_event = trace::Span_::Event::default();

        pb_event.time_unix_nano = event.occurs_at.duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos() as u64;
        pb_event.name = event.name.to_string();
        pb_event.attributes = event.attributes.iter().map(map_kv).collect();

        span.events.push(pb_event);
        Ok(())
    }

    fn close_span(&mut self, idx: SpanCollectionIndex, closed_at: SystemTime) {
        let Some(span) = self.get_open_span_mut(idx) else { return };
        span.end_time_unix_nano = closed_at.duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos() as u64;
    }

    fn drop_span(&mut self,
        idx: SpanCollectionIndex,
        dropped_at: SystemTime,
        export: impl Fn(Self::Exportable),
        _private: PrivateMarker
    ) {
        let Some(mut span) = self.spans[idx.0 as usize].take() else {
            return // no span here TODO should log error
        };
        self.free_indicies.push(idx.0);

        if span.end_time_unix_nano == 0 {
            span.end_time_unix_nano = dropped_at.duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos() as u64;
        }

        let scope_span = &mut self.dropped_spans.scope_spans[0];
        scope_span.spans.push(span);
        if let Some(size) = self.config.autoflush_batch_size {
            if scope_span.spans.len() > size as usize {
                self.flush(export);
            }
        }
    }
    
    fn flush(&mut self, export: impl Fn(Self::Exportable)) {
        struct PbIoWrite(Vec<u8>);

        impl micropb::PbWrite for PbIoWrite {
            type Error = std::io::Error;
            fn pb_write(&mut self, data: &[u8]) -> Result<(), Self::Error> {
                let n = self.0.write(data)?;
                assert_eq!(n, data.len());
                Ok(())
            }
        }

        let mut encoder = micropb::PbEncoder::new(PbIoWrite(vec![]));
        self.dropped_spans.encode(&mut encoder).unwrap(); // TODO handle error
        self.dropped_spans.scope_spans[0].spans.clear();
        export(encoder.into_writer().0);
    }
}