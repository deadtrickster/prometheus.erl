Prometheus.io client in Erlang
=====

Example Console Session
-----
Run shell with compiled and loaded app:

    $ rebar3 shell

Start prometheus app:
``` erlang
prometheus:start().
```
Register metrics:
```erlang
prometheus_gauge:register([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
prometheus_counter:register([{name, http_requests_total}, {help, "Http request count"}]).
```
Use metrics:
```erlang
prometheus_gauge:set(pool_size, 365),
prometheus_counter:inc(http_requests_total).
```

Export metrics as text:
```erlang
io:format(prometheus_text_format:format()).
```
->
```
# TYPE http_requests_total counter
# HELP http_requests_total Http request count
http_requests_total 1
# TYPE pool_size gauge
# HELP pool_size MongoDB Connections pool size
pool_size 365
```
Implemented Metrics
 - [x] Counter
 - [x] Gauge
 - [ ] Summary
 - [ ] Histogram

Custom Collectors
-----
  - `erlang_vm_memory_collector` - Collects information about Erlang VM memory usage mainly using `erlang:memory/0`
  - `erlang_vm_statistics_collector` - Collects Erlang VM statistics using `erlang:statistics/1`

Configuration
-----
Prometheus.erl supports standard Erlang app configuration.
  - `default_collectors` - List of custom collectors modules to be registered automatically. Defaults to `?PROMETHEUS_DEFAULT_COLLECTORS`
  - `default_metrics` - List of metrics to be registered during app startup. Metric format: `{Registry, Metric, Spec}` where `Registry` is registry name, `Metric` is metric type (prometheus_counter, prometheus_gauge ... etc), `Spec` is a list to be passed to `Metric:register/2`.

Build
-----

    $ rebar3 compile
