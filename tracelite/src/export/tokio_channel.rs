use crate::tracer::globals;
use super::Exporter;

pub async fn run_tokio_export_loop<B>(
    mut batch_receiver: tokio::sync::mpsc::UnboundedReceiver<B>,
    exporter: impl Exporter<B>,
    tracer_autoflush_interval: std::time::Duration,
){
    loop {
        let autoflush = tokio::time::sleep(tracer_autoflush_interval);
        tokio::select! {
            opt = batch_receiver.recv() => {
                match opt {
                    Some(batch) => {
                        #[cfg(feature = "log")]
                        log::debug!("tokio background worker received batch");
                        exporter.export(&batch).await
                    }
                    None => {
                        #[cfg(feature = "log")]
                        log::debug!("tokio background worker has dead sender");
                        return // channel senders dropped
                    }
                }
            }
            _ = autoflush => {
                #[cfg(feature = "log")]
                log::debug!("tokio background worker performs time-based autoflush");
                globals::tracer().ok().map(|t| t.flush());
            }
        };
    }
}

pub fn spawn_tokio_export_task<B: Send + 'static>(
    exporter: impl Exporter<B>,
    tracer_autoflush_interval: std::time::Duration,
) -> impl Fn(B) {
    let (batch_sender, batch_receiver) = tokio::sync::mpsc::unbounded_channel();

    tokio::spawn(async move {
        run_tokio_export_loop(batch_receiver, exporter, tracer_autoflush_interval).await
    });

    move |batch| {
        // TODO how to react to dropped receiver?
        match batch_sender.send(batch) {
            Ok(()) => println!("[DEBUG] tracelite: sent batch to background worker"),
            Err(err) => eprintln!("[ERROR] tracelite: failed to send batch to background worker: {err}")
        }
    }
}