#[macro_use] extern crate tracelite;

use opentelemetry_micropb::std::resource_::v1_::Resource;
use tracelite::clocks::TestClock;
use tracelite::export::{spawn_tokio_export_task, TestExport};
use tracelite::id_generators::FastrandIdGenerator;
use tracelite::sampling::{AlwaysSampler, Sampler, SamplingDecision, SamplingResult};
use tracelite::span_collections::SpanCollectionConfig;
use tracelite::{micropb_h2grpc_config, sampling, AttributeList, AttributeValue, DefaultTracerConfig, Severity};
use std::process::id;
use std::time::Duration;
use std::{error::Error, io};

#[info_span(device)]
async fn check_device(device: &str){
    info_event!("DEVICE {device} INFO");
    debug_event!("DEVICE {device} DEBUG");
    some_operation(device).await;
}

#[debug_span]
async fn some_operation(device: &str){
    info_event!("DEVICE {device} SUBOP INFO");
    debug_event!("DEVICE {device} SUBOP DEBUG");

    some_operation2(device).await
}

#[info_span]
async fn some_operation2(device: &str){
    info_event!("DEVICE {device} SUBOP2 INFO");
    debug_event!("DEVICE {device} SUBOP2 DEBUG");
}

pub struct CustomSampler;

impl Sampler for CustomSampler {
    fn should_sample(&self, args: &tracelite::SpanArgs) -> SamplingResult {
        if args.parent.as_ref().is_some() {
            println!("SPAN {:?} INHERITS FROM PARENT", args.name.as_str());
            return SamplingDecision::RecordAndSample.into()
        }

        println!("LEN {}", args.attributes.0.len());

        let detail_enabled = args.attributes.get("device")
            .into_iter()
            .any(|v| matches!(v, AttributeValue::Str("traceme")));

        println!("SPAN {:?} DETAIL ENABLED: {detail_enabled}", args.name.as_str());

        if detail_enabled {
            SamplingResult{
                decision: SamplingDecision::RecordAndSample,
                dyn_trace_detail: Some(Severity::Debug.into()),
            }
        } else {
            SamplingResult{
                decision: SamplingDecision::RecordAndSample,
                dyn_trace_detail: Some(Severity::Info.into()),
            }
        }

    }
}

#[tokio::main]
async fn main(){
    let test_clock = TestClock::default();
    let test_export = TestExport::default();

    DefaultTracerConfig::new(
        test_clock.clone(),
        FastrandIdGenerator,
        AlwaysSampler,
        CustomSampler,
        SpanCollectionConfig::new("resource", AttributeList(&[])).build(),
        spawn_tokio_export_task(test_export.clone(), Duration::from_secs(5)),
    ).install();

    test_clock.advance(1000);

    // check_device("traceme").await;
    check_device("tracemeNOT").await;

    std::thread::sleep(std::time::Duration::new(10, 0));

    println!("{:?}", test_export.requests.lock().await);
}