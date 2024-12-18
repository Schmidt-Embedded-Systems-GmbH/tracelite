use super::SpanExporter;
use opentelemetry_micropb::std::collector_::trace_::v1_::ExportTraceServiceRequest;
use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct TestExport {
    pub requests: Arc<tokio::sync::Mutex<Vec<ExportTraceServiceRequest>>>
}

impl SpanExporter<ExportTraceServiceRequest> for TestExport {
    fn export<'a>(&'a self, batch: &'a ExportTraceServiceRequest) -> impl std::future::Future<Output = ()> + Send + 'a {
        async {
            self.requests.lock().await.push(batch.clone());
        }
    }
}