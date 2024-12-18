use crate::tracer::globals;
use super::SpanExporter;

pub async fn run_tokio_export_loop(
    mut batch_receiver: tokio::sync::mpsc::UnboundedReceiver<Vec<u8>>,
    exporter: impl SpanExporter,
    tracer_autoflush_interval: std::time::Duration,
){
    loop {
        let autoflush = tokio::time::sleep(tracer_autoflush_interval);
        tokio::select! {
            opt = batch_receiver.recv() => {
                println!("[DEBUG] tracelite: background worker received batch or dead sender");
                match opt {
                    Some(batch) => exporter.export(&batch).await,
                    None => return // channel senders dropped
                }
            }
            _ = autoflush => {
                println!("[DEBUG] tracelite: background worker performs autoflush");
                globals::tracer().ok().map(|t| t.flush());
            }
        };
    }
}

pub fn spawn_tokio_export_task(
    exporter: impl SpanExporter,
    tracer_autoflush_interval: std::time::Duration,
) -> impl Fn(Vec<u8>)
{
    let (batch_sender, batch_receiver) = tokio::sync::mpsc::unbounded_channel();

    tokio::spawn(async move {
        run_tokio_export_loop(batch_receiver, exporter, tracer_autoflush_interval).await
    });

    move |batch| {
        // TODO how to react to dropped receiver?
        let batch_size = batch.len();
        match batch_sender.send(batch) {
            Ok(()) => println!("[DEBUG] tracelite: sent batch of size {batch_size} to background worker"),
            Err(err) => eprintln!("[ERROR] tracelite: failed to send batch to background worker: {err}")
        }
    }
}