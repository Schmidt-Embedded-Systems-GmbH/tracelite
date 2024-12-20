mod tokio;
pub use tokio::{run_tokio_export_loop, spawn_tokio_export_task};

mod h2grpc;
pub use h2grpc::H2GrpcExport;

pub trait SpanExporter<B>: Send + 'static {
    fn export<'a>(&'a self, batch: &'a B) -> impl std::future::Future<Output = ()> + Send + 'a;
}

use opentelemetry_micropb::std::collector_::trace_::v1_::ExportTraceServiceRequest;
use ::tokio::sync::Mutex;
use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct TestExport {
    pub requests: Arc<Mutex<Vec<ExportTraceServiceRequest>>>
}

impl SpanExporter<ExportTraceServiceRequest> for TestExport {
    fn export<'a>(&'a self, batch: &'a ExportTraceServiceRequest) -> impl std::future::Future<Output = ()> + Send + 'a {
        async {
            self.requests.lock().await.push(batch.clone());
        }
    }
}