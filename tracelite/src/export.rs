use super::internal::globals;
use std::time::Duration;

pub async fn tokio_export_loop<F>(
    mut batch_receiver: tokio::sync::mpsc::UnboundedReceiver<Vec<u8>>,
    http_post: impl Fn(&[u8]) -> F,
    tracer_autoflush_interval: Duration,
)
    where F: std::future::Future<Output = ()>
{
    loop {
        let autoflush = tokio::time::sleep(tracer_autoflush_interval);
        tokio::select! {
            opt = batch_receiver.recv() => {
                match opt {
                    Some(batch) => http_post(&batch).await,
                    None => return // channel senders dropped
                }
            }
            _ = autoflush => {
                globals::tracer().map(|t| t.flush());
            }
        };
    }
}

pub async fn spawn_tokio_export_loop<F>(
    http_post: impl Fn(&[u8]) -> F + Send + 'static,
    tracer_autoflush_interval: Duration,
) -> impl Fn(Vec<u8>)
    where F: std::future::Future<Output = ()> + Send + 'static
{
    let (batch_sender, batch_receiver) = tokio::sync::mpsc::unbounded_channel();

    tokio::spawn(async move {
        tokio_export_loop(batch_receiver, http_post, tracer_autoflush_interval)
    });

    move |batch| {
        // TODO how to react to dropped receiver?
        let _ = batch_sender.send(batch);
    }
}