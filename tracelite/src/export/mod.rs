mod tokio;
pub use tokio::{run_tokio_export_loop, spawn_tokio_export_task};

mod h2grpc;
pub use h2grpc::H2GrpcExport;

mod test;
pub use test::TestExport;

pub trait SpanExporter<B>: Send + 'static {
    fn export<'a>(&'a self, batch: &'a B) -> impl std::future::Future<Output = ()> + Send + 'a;
}