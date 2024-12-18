use super::SpanExporter;
use http::header::CONTENT_TYPE;
use http::Method;

// TODO keep connection open
// TODO get rid of unwraps
pub struct H2GrpcExport {
    grpc_method_uri: http::Uri,
    host: String,
    port: u16,
}

impl SpanExporter for H2GrpcExport {
    fn export<'a>(&'a self, data: &'a [u8]) -> impl std::future::Future<Output = ()> + Send + 'a {
        let data = bytes::Bytes::copy_from_slice(data);
        async {
            if let Err(err) = self.try_send(data).await {
                println!("[ERROR] tracelite: failed to export batch: {err}");
            }
        }
    }
}

impl H2GrpcExport {
    pub fn new(otlp_endpoint: &str) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let uri: http::Uri = otlp_endpoint.parse().unwrap();
        let (host, port) = (uri.host().unwrap().to_owned(), uri.port_u16().unwrap_or(4317));

        let insert_slash = if otlp_endpoint.ends_with("/") { "" } else { "/" };
        let method_name = "opentelemetry.proto.collector.trace.v1.TraceService/Export";
        let grpc_method_uri = format!("{otlp_endpoint}{insert_slash}{method_name}").parse().unwrap();

        Ok(Self { grpc_method_uri, host, port })
    }

    async fn try_send(&self, data: bytes::Bytes) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut headers = http::HeaderMap::new();
        headers.insert(CONTENT_TYPE, "application/grpc".parse().unwrap());
        headers.insert("te", "trailers".parse().unwrap());

        println!("[DEBUG] tracelite: connecting to host {}:{} for endpoint {}", self.host, self.port, self.grpc_method_uri);
        if self.port == 4318 {
            println!("[WARN] tracelite: exporting to port 4318, are you sure? (opentelemetry-collector uses 4317 by-default for gRPC)");
        }

        let tcp = tokio::net::TcpStream::connect((self.host.as_str(), self.port)).await?;
        let (mut h2, connection) = h2::client::handshake(tcp).await?;
        tokio::spawn(async move {
            match connection.await {
                Ok(()) => println!("[DEBUG] tracelite: connection established"),
                Err(err) => println!("[DEBUG] tracelite: connection failure: {err}"),
            }
        });

        let mut req_headers = http::Request::new(());
        *req_headers.method_mut() = Method::POST;
        *req_headers.uri_mut() = self.grpc_method_uri.clone();
        req_headers.headers_mut().extend(headers);
            
        let (response, mut stream) = h2.send_request(req_headers, false)?;
        stream.send_data(data, true)?;

        let resp = response.await?;

        println!("[DEBUG] tracelite: received response (status {}) with headers/trailers {:?}", resp.status(), resp.headers());

        Ok(())
    }
}