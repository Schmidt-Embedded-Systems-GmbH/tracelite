#!/bin/bash

set +x

if [ "$(uname)" == "Darwin" ]; then
    wget --verbose 'https://github.com/open-telemetry/opentelemetry-collector-releases/releases/download/v0.106.1/otelcol-contrib_0.106.1_darwin_amd64.tar.gz' -O otelcol.tar.gz     
else
    wget --verbose 'https://github.com/open-telemetry/opentelemetry-collector-releases/releases/download/v0.106.1/otelcol-contrib_0.106.1_linux_amd64.tar.gz' -O otelcol.tar.gz    
fi

tar -xvzf otelcol.tar.gz otelcol-contrib
