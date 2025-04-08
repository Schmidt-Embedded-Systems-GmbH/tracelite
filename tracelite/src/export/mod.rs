mod tokio_channel;
pub use tokio_channel::{run_tokio_export_loop, spawn_tokio_export_task};

pub mod h2grpc;

use std::time::Duration;

pub trait ExporterConfig {
    type Exportable;
    // type ExportSink: Fn(Self::Exportable) + Send + Sync + 'static;
    fn new(endpoint_url: String, autoflush_interval: Duration) -> Self;
    fn build(self) -> impl Fn(Self::Exportable) + Send + Sync + 'static;
}

pub trait Exporter<B>: Send + 'static {
    fn export<'a>(&'a self, batch: &'a B) -> impl std::future::Future<Output = ()> + Send + 'a;
}

// TODO move into dedicated file
use opentelemetry_micropb::std::collector_::trace_::v1_::ExportTraceServiceRequest;
use tokio::sync::Mutex;
use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct TestExport {
    pub requests: Arc<Mutex<Vec<ExportTraceServiceRequest>>>
}

impl Exporter<ExportTraceServiceRequest> for TestExport {
    fn export<'a>(&'a self, batch: &'a ExportTraceServiceRequest) -> impl std::future::Future<Output = ()> + Send + 'a {
        async {
            self.requests.lock().await.push(batch.clone());
        }
    }
}