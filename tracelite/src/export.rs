use super::tracer::globals;
use std::time::Duration;

pub trait Export: Send + 'static {
    fn export<'a>(&'a self, data: &'a [u8]) -> impl std::future::Future<Output = ()> + Send + 'a;
}

pub async fn tokio_export_loop(
    mut batch_receiver: tokio::sync::mpsc::UnboundedReceiver<Vec<u8>>,
    export: impl Export,
    tracer_autoflush_interval: Duration,
){
    loop {
        let autoflush = tokio::time::sleep(tracer_autoflush_interval);
        tokio::select! {
            opt = batch_receiver.recv() => {
                match opt {
                    Some(batch) => export.export(&batch).await,
                    None => return // channel senders dropped
                }
            }
            _ = autoflush => {
                globals::tracer().map(|t| t.flush());
            }
        };
    }
}

pub fn spawn_tokio_export_loop(
    export: impl Export,
    tracer_autoflush_interval: Duration,
) -> impl Fn(Vec<u8>)
{
    let (batch_sender, batch_receiver) = tokio::sync::mpsc::unbounded_channel();

    tokio::spawn(async move {
        tokio_export_loop(batch_receiver, export, tracer_autoflush_interval)
    });

    move |batch| {
        // TODO how to react to dropped receiver?
        let _ = batch_sender.send(batch);
    }
}

pub struct ReqwestPost {
    pub otlp_endpoint: String,
    pub client: reqwest::Client
}

impl Export for ReqwestPost {
    fn export<'a>(&'a self, data: &'a [u8]) -> impl std::future::Future<Output = ()> + 'a {
        use futures::FutureExt;
        let url = format!("{}/v1/traces", self.otlp_endpoint);
        let client = self.client.clone();
        client.post(url)
            .body(data.to_vec()) // FIXME can we avoid this allocation?
            .send()
            .map(|result| {
                match result {
                    Ok(_) => {
                        // TODO make use of ExportTraces response
                        println!("tracelite: exported {} bytes", data.len());
                    }
                    Err(err) => {
                        eprintln!("tracelite: failed to export {} bytes: {err}", data.len());
                    }
                }
            })
    }
}

// pub fn http_tcp_stream_post(host: &str) -> impl Fn(&[u8]) + Send + 'static {
//     let url = url.to_owned();
//     move |data| {
//         let mut stream = match std::net::TcpStream::connect(&url) {
//             Ok(stream) => stream,
//             Err(err) => {
//                 eprint!("tracelite: failed to open tcp stream to {url}/v1/traces: {err}");
//                 return
//             }
//         };

//         let preamble = format!(
//             "POST /v1/traces HTTP/1.0\r\n\
//             Content-Length: {}\r\n\
//             ",
//         ); 
//         if let Err(err) = stream.write(data) {
//             eprint!("tracelite: failed to write request preamble to tcp stream: {err}");
//             return
//         }

//         if let Err(err) = stream.write(data) {
//             eprint!("tracelite: failed to write request body to tcp stream: {err}");
//             return
//         }
        
//         let mut resp_buf = vec![]; // TODO what to do with response?
//         if let Err(err) = stream.read_to_end(buf) {

//             eprint!("tracelite: failed to write to tcp stream: {err}");
//             return
//         }
//     }
// }