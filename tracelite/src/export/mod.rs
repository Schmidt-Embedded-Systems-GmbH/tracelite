mod tokio;
pub use tokio::{run_tokio_export_loop, spawn_tokio_export_task};

mod h2grpc;
pub use h2grpc::H2GrpcExport;


pub trait SpanExporter: Send + 'static {
    fn export<'a>(&'a self, data: &'a [u8]) -> impl std::future::Future<Output = ()> + Send + 'a;
}