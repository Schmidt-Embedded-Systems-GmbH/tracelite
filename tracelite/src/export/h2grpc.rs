use std::time::Duration;

use bytes::{Bytes, BytesMut};
use http::header::CONTENT_TYPE;
use http::Method;
use micropb::MessageEncode;
use opentelemetry_micropb::std::collector_::trace_::v1_::ExportTraceServiceRequest;

use super::spawn_tokio_export_task;

pub struct ExporterConfig {
    endpoint_url: String,
    autoflush_interval: Duration,
}

impl super::ExporterConfig for ExporterConfig {
    type Exportable = ExportTraceServiceRequest;
    fn new(endpoint_url: String, autoflush_interval: Duration) -> Self {
        Self{ endpoint_url, autoflush_interval }
    }
    fn build(self) -> impl Fn(Self::Exportable) + Send + Sync + 'static {
        let exporter = Exporter::new(&self.endpoint_url).unwrap();
        spawn_tokio_export_task(exporter, self.autoflush_interval)
    }
}

// TODO keep connection open
// TODO get rid of unwraps
pub struct Exporter {
    grpc_method_uri: http::Uri,
    host: String,
    port: u16,
}

impl Exporter {
    pub fn new(otlp_endpoint: &str) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let uri: http::Uri = otlp_endpoint.parse().unwrap();
        let parts = uri.into_parts();
        
        // TODO assert scheme is http://
        let authority = parts.authority.unwrap();
        let path_and_query = parts.path_and_query.unwrap();

        let host = authority.host().to_owned();
        let port = authority.port_u16().unwrap_or(4317);
        let method_name = "opentelemetry.proto.collector.trace.v1.TraceService/Export";

        let authority_str = format!("{host}:{port}");
        let method_path = if path_and_query.path().ends_with("/") {
            format!("{}{method_name}", path_and_query.path())
        } else {
            format!("{}/{method_name}", path_and_query.path())
        };
        let method_path_and_query = if let Some(query) = path_and_query.query() {
            format!("{method_path}?{query}")
        } else {
            method_path
        };

        let grpc_method_uri = http::Uri::builder()
            .scheme("http")
            .authority(authority_str)
            .path_and_query(method_path_and_query)
            .build()
            .unwrap();

        Ok(Self { grpc_method_uri, host, port })
    }
}

impl super::Exporter<ExportTraceServiceRequest> for Exporter {
    fn export<'a>(&'a self, batch: &'a ExportTraceServiceRequest) -> impl std::future::Future<Output = ()> + Send + 'a {
        // let data = bytes::Bytes::copy_from_slice(data);
        struct PbIoWrite(BytesMut);

        impl micropb::PbWrite for PbIoWrite {
            type Error = std::io::Error;
            fn pb_write(&mut self, data: &[u8]) -> Result<(), Self::Error> {
                self.0.extend_from_slice(data);
                Ok(())
            }
        }

        // gRPC framing: 1 byte flag (compressed=0) + 4 byte message length (big endian)
        let mut buf = bytes::BytesMut::new();
        buf.extend_from_slice(&[0u8, 0, 0, 0, 0]); // NOTE will set message length later
        let mut encoder = micropb::PbEncoder::new(PbIoWrite(buf));
        batch.encode(&mut encoder).unwrap(); // TODO handle error

        let mut buf = encoder.into_writer().0;
        let message_len = buf.len() as u32 - 5;
        buf[1..5].copy_from_slice(&message_len.to_be_bytes());

        async {
            if let Err(_err) = self.try_send_std(buf.into()).await {
                #[cfg(feature = "log")]
                log::error!("failed to export batch: {_err}");
            }
        }
    }
}

impl Exporter {
    async fn try_send_std(&self, data: Bytes) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut headers = http::HeaderMap::new();
        headers.insert(CONTENT_TYPE, "application/grpc".parse().unwrap());
        headers.insert("te", "trailers".parse().unwrap());

        #[cfg(feature = "log")]
        log::info!("connecting to host {}:{} for endpoint {}", self.host, self.port, self.grpc_method_uri);
        if self.port == 4318 {
            #[cfg(feature = "log")]
            log::warn!("exporting to port 4318, are you sure? (opentelemetry-collector uses 4317 by-default for gRPC)");
        }

        let tcp = tokio::net::TcpStream::connect((self.host.as_str(), self.port)).await?;
        let (mut h2, connection) = h2::client::handshake(tcp).await?;
        tokio::spawn(async move {
            match connection.await {
                Ok(()) => {
                    #[cfg(feature = "log")]
                    log::info!("connection established")
                }
                Err(_err) => {
                    #[cfg(feature = "log")]
                    log::info!("connection failure: {_err}")
                }
            }
        });

        let mut req_headers = http::Request::new(());
        *req_headers.method_mut() = Method::POST;
        *req_headers.uri_mut() = self.grpc_method_uri.clone();
        req_headers.headers_mut().extend(headers);

        #[cfg(feature = "log")]
        log::debug!("sending {} bytes of trace data", data.len());
            
        let (response, mut stream) = h2.send_request(req_headers, false)?;
        stream.send_data(data, true)?;

        let _resp = response.await?;

        #[cfg(feature = "log")]
        log::debug!("received response (status {}) with headers/trailers {:?}", _resp.status(), _resp.headers());

        Ok(())
    }
}