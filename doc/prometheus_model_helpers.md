

# Module prometheus_model_helpers #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Helpers for working with Prometheus data model.

<a name="description"></a>

## Description ##
For advanced users.
Probably will be used with [`prometheus_collector`](prometheus_collector.md).
<a name="types"></a>

## Data Types ##




### <a name="type-buckets">buckets()</a> ###


<pre><code>
buckets() = [{<a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_buckets.html#type-bucket_bound">prometheus_buckets:bucket_bound()</a>, non_neg_integer()}, ...]
</code></pre>




### <a name="type-counter">counter()</a> ###


<pre><code>
counter() = <a href="#type-value">value()</a> | {<a href="#type-value">value()</a>} | {<a href="#type-labels">labels()</a>, <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-gauge">gauge()</a> ###


<pre><code>
gauge() = <a href="#type-value">value()</a> | {<a href="#type-value">value()</a>} | {<a href="#type-labels">labels()</a>, <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-histogram">histogram()</a> ###


<pre><code>
histogram() = {<a href="#type-buckets">buckets()</a>, non_neg_integer(), <a href="#type-value">value()</a>} | {<a href="#type-labels">labels()</a>, <a href="#type-buckets">buckets()</a>, non_neg_integer(), <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-label">label()</a> ###


<pre><code>
label() = {<a href="#type-label_name">label_name()</a>, <a href="#type-label_value">label_value()</a>}
</code></pre>




### <a name="type-label_name">label_name()</a> ###


<pre><code>
label_name() = term()
</code></pre>




### <a name="type-label_value">label_value()</a> ###


<pre><code>
label_value() = term()
</code></pre>




### <a name="type-labels">labels()</a> ###


<pre><code>
labels() = [<a href="#type-label">label()</a>] | <a href="#type-pre_rendered_labels">pre_rendered_labels()</a>
</code></pre>




### <a name="type-metrics">metrics()</a> ###


<pre><code>
metrics() = <a href="#type-tmetric">tmetric()</a> | [<a href="#type-tmetric">tmetric()</a>]
</code></pre>




### <a name="type-pbool">pbool()</a> ###


<pre><code>
pbool() = <a href="#type-prometheus_boolean">prometheus_boolean()</a> | {<a href="#type-prometheus_boolean">prometheus_boolean()</a>} | {<a href="#type-labels">labels()</a>, <a href="#type-prometheus_boolean">prometheus_boolean()</a>}
</code></pre>




### <a name="type-pre_rendered_labels">pre_rendered_labels()</a> ###


<pre><code>
pre_rendered_labels() = binary()
</code></pre>




### <a name="type-prometheus_boolean">prometheus_boolean()</a> ###


<pre><code>
prometheus_boolean() = boolean() | number() | list() | undefined
</code></pre>




### <a name="type-summary">summary()</a> ###


<pre><code>
summary() = {non_neg_integer(), <a href="#type-value">value()</a>} | {<a href="#type-labels">labels()</a>, non_neg_integer(), <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-tmetric">tmetric()</a> ###


<pre><code>
tmetric() = <a href="#type-gauge">gauge()</a> | <a href="#type-counter">counter()</a> | <a href="#type-untyped">untyped()</a> | <a href="#type-summary">summary()</a> | <a href="#type-histogram">histogram()</a> | <a href="#type-pbool">pbool()</a>
</code></pre>




### <a name="type-untyped">untyped()</a> ###


<pre><code>
untyped() = <a href="#type-value">value()</a> | {<a href="#type-value">value()</a>} | {<a href="#type-labels">labels()</a>, <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = float() | integer() | undefined | infinity
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#boolean_metric-1">boolean_metric/1</a></td><td>
Equivalent to
<a href="#boolean_metric-2"><tt>boolean_metric(Labels, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#boolean_metric-2">boolean_metric/2</a></td><td>
Creates boolean metric with <code>Labels</code> and <code>Value</code>.</td></tr><tr><td valign="top"><a href="#boolean_metrics-1">boolean_metrics/1</a></td><td>Equivalent to
<a docgen-rel="seemfa" docgen-href="#boolean_metric/1" href="#boolean_metric-1"><code>lists:map(fun boolean_metric/1, Values)</code></a>.</td></tr><tr><td valign="top"><a href="#counter_metric-1">counter_metric/1</a></td><td>
Equivalent to
<a href="#counter_metric-2"><tt>counter_metric(Labels, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#counter_metric-2">counter_metric/2</a></td><td>
Creates counter metric with <code>Labels</code> and <code>Value</code>.</td></tr><tr><td valign="top"><a href="#counter_metrics-1">counter_metrics/1</a></td><td>Equivalent to
<a docgen-rel="seemfa" docgen-href="#counter_metric/1" href="#counter_metric-1"><code>lists:map(fun counter_metric/1, Specs)</code></a>.</td></tr><tr><td valign="top"><a href="#create_mf-4">create_mf/4</a></td><td>
Create Metric Family of <code>Type</code>, <code>Name</code> and <code>Help</code>.</td></tr><tr><td valign="top"><a href="#create_mf-5">create_mf/5</a></td><td>
Create Metric Family of <code>Type</code>, <code>Name</code> and <code>Help</code>.</td></tr><tr><td valign="top"><a href="#gauge_metric-1">gauge_metric/1</a></td><td>
Equivalent to
<a href="#gauge_metric-2"><tt>gauge_metric(Labels, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#gauge_metric-2">gauge_metric/2</a></td><td>
Creates gauge metric with <code>Labels</code> and <code>Value</code>.</td></tr><tr><td valign="top"><a href="#gauge_metrics-1">gauge_metrics/1</a></td><td>Equivalent to
<a docgen-rel="seemfa" docgen-href="#gauge_metric/1" href="#gauge_metric-1"><code>lists:map(fun gauge_metric/1, Values)</code></a>.</td></tr><tr><td valign="top"><a href="#histogram_metric-1">histogram_metric/1</a></td><td>
Equivalent to
<a href="#histogram_metric-3=4">
<tt>histogram_metric(Labels, Buckets, Count, Sum)</tt></a>.</td></tr><tr><td valign="top"><a href="#histogram_metric-3">histogram_metric/3</a></td><td>Equivalent to <a href="#histogram_metric-4"><tt>histogram_metric([], Buckets, Count, Sum)</tt></a>.</td></tr><tr><td valign="top"><a href="#histogram_metric-4">histogram_metric/4</a></td><td>
Creates histogram metric with <code>Labels</code>, <code>Buckets</code>, <code>Count</code> and <code>Sum</code>.</td></tr><tr><td valign="top"><a href="#histogram_metrics-1">histogram_metrics/1</a></td><td>Equivalent to
<a docgen-rel="seemfa" docgen-href="#histogram_metric/1" href="#histogram_metric-1"><code>lists:map(fun histogram_metric/1, Specs)</code></a>.</td></tr><tr><td valign="top"><a href="#label_pair-1">label_pair/1</a></td><td>
Creates <code>prometheus_model:</code>LabelPair'()' from {Name, Value} tuple.</td></tr><tr><td valign="top"><a href="#label_pairs-1">label_pairs/1</a></td><td>Equivalent to
<a docgen-rel="seemfa" docgen-href="#label_pair/1" href="#label_pair-1"><code>lists:map(fun label_pair/1, Labels)</code></a>.</td></tr><tr><td valign="top"><a href="#metric_name-1">metric_name/1</a></td><td>
If <code>Name</code> is a list, looks for atoms and converts them to binaries.</td></tr><tr><td valign="top"><a href="#summary_metric-1">summary_metric/1</a></td><td>
Equivalent to
<a href="#summary_metric-3"><tt>summary_metric(Labels, Count, Sum)</tt></a>.</td></tr><tr><td valign="top"><a href="#summary_metric-2">summary_metric/2</a></td><td>Equivalent to <a href="#summary_metric-3"><tt>summary_metric([], Count, Sum)</tt></a>.</td></tr><tr><td valign="top"><a href="#summary_metric-3">summary_metric/3</a></td><td>Equivalent to <a href="#summary_metric-4"><tt>summary_metric([], Count, Sum, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#summary_metric-4">summary_metric/4</a></td><td>
Creates summary metric with <code>Labels</code>, <code>Count</code> and <code>Sum</code>.</td></tr><tr><td valign="top"><a href="#summary_metrics-1">summary_metrics/1</a></td><td>Equivalent to
<a docgen-rel="seemfa" docgen-href="#summary_metric/1" href="#summary_metric-1"><code>lists:map(fun summary_metric/1, Specs)</code></a>.</td></tr><tr><td valign="top"><a href="#untyped_metric-1">untyped_metric/1</a></td><td>
Equivalent to
<a href="#untyped_metric-2"><tt>untyped_metric(Labels, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#untyped_metric-2">untyped_metric/2</a></td><td>
Creates untyped metric with <code>Labels</code> and <code>Value</code>.</td></tr><tr><td valign="top"><a href="#untyped_metrics-1">untyped_metrics/1</a></td><td>Equivalent to
<a docgen-rel="seemfa" docgen-href="#untyped_metric/1" href="#untyped_metric-1"><code>lists:map(fun untyped_metric/1, Values)</code></a>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="boolean_metric-1"></a>

### boolean_metric/1 ###

<pre><code>
boolean_metric(Boolean) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Boolean = <a href="#type-pbool">pbool()</a></code></li></ul>

Equivalent to
[`boolean_metric(Labels, Value)`](#boolean_metric-2).

<a name="boolean_metric-2"></a>

### boolean_metric/2 ###

<pre><code>
boolean_metric(Labels, Value) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = <a href="#type-labels">labels()</a></code></li><li><code>Value = <a href="#type-prometheus_boolean">prometheus_boolean()</a></code></li></ul>

Creates boolean metric with `Labels` and `Value`.

<a name="boolean_metrics-1"></a>

### boolean_metrics/1 ###

`boolean_metrics(Values) -> any()`

Equivalent to
[`lists:map(fun boolean_metric/1, Values)`](#boolean_metric-1).

<a name="counter_metric-1"></a>

### counter_metric/1 ###

<pre><code>
counter_metric(Spec) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Spec = <a href="#type-counter">counter()</a></code></li></ul>

Equivalent to
[`counter_metric(Labels, Value)`](#counter_metric-2).

<a name="counter_metric-2"></a>

### counter_metric/2 ###

<pre><code>
counter_metric(Labels, Value) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = <a href="#type-labels">labels()</a></code></li><li><code>Value = <a href="#type-value">value()</a></code></li></ul>

Creates counter metric with `Labels` and `Value`.

<a name="counter_metrics-1"></a>

### counter_metrics/1 ###

`counter_metrics(Specs) -> any()`

Equivalent to
[`lists:map(fun counter_metric/1, Specs)`](#counter_metric-1).

<a name="create_mf-4"></a>

### create_mf/4 ###

<pre><code>
create_mf(Name, Help, Type, Metrics) -&gt; MetricFamily
</code></pre>

<ul class="definitions"><li><code>Name = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_metric.html#type-name">prometheus_metric:name()</a></code></li><li><code>Help = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_metric.html#type-help">prometheus_metric:help()</a></code></li><li><code>Type = atom()</code></li><li><code>Metrics = [<a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>] | <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a> | <a href="#type-metrics">metrics()</a></code></li><li><code>MetricFamily = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-MetricFamily">prometheus_model:'MetricFamily'()</a></code></li></ul>

Create Metric Family of `Type`, `Name` and `Help`.
`Collector:collect_metrics/2` callback will be called and expected to
return individual metrics list.

<a name="create_mf-5"></a>

### create_mf/5 ###

<pre><code>
create_mf(Name, Help, Type, Collector, CollectorData) -&gt; MetricFamily
</code></pre>

<ul class="definitions"><li><code>Name = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_metric.html#type-name">prometheus_metric:name()</a></code></li><li><code>Help = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_metric.html#type-help">prometheus_metric:help()</a></code></li><li><code>Type = atom()</code></li><li><code>Collector = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_collector.html#type-collector">prometheus_collector:collector()</a></code></li><li><code>CollectorData = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_collector.html#type-data">prometheus_collector:data()</a></code></li><li><code>MetricFamily = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-MetricFamily">prometheus_model:'MetricFamily'()</a></code></li></ul>

Create Metric Family of `Type`, `Name` and `Help`.
`Collector:collect_metrics/2` callback will be called and expected to
return individual metrics list.

<a name="gauge_metric-1"></a>

### gauge_metric/1 ###

<pre><code>
gauge_metric(Gauge) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Gauge = <a href="#type-gauge">gauge()</a></code></li></ul>

Equivalent to
[`gauge_metric(Labels, Value)`](#gauge_metric-2).

<a name="gauge_metric-2"></a>

### gauge_metric/2 ###

<pre><code>
gauge_metric(Labels, Value) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = <a href="#type-labels">labels()</a></code></li><li><code>Value = <a href="#type-value">value()</a></code></li></ul>

Creates gauge metric with `Labels` and `Value`.

<a name="gauge_metrics-1"></a>

### gauge_metrics/1 ###

`gauge_metrics(Values) -> any()`

Equivalent to
[`lists:map(fun gauge_metric/1, Values)`](#gauge_metric-1).

<a name="histogram_metric-1"></a>

### histogram_metric/1 ###

<pre><code>
histogram_metric(Histogram) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Histogram = <a href="#type-histogram">histogram()</a></code></li></ul>

Equivalent to
[
`histogram_metric(Labels, Buckets, Count, Sum)`](#histogram_metric-3=4).

<a name="histogram_metric-3"></a>

### histogram_metric/3 ###

`histogram_metric(Buckets, Count, Sum) -> any()`

Equivalent to [`histogram_metric([], Buckets, Count, Sum)`](#histogram_metric-4).

<a name="histogram_metric-4"></a>

### histogram_metric/4 ###

<pre><code>
histogram_metric(Labels, Buckets, Count, Sum) -&gt; Metric
</code></pre>

<ul class="definitions"><li><code>Labels = <a href="#type-labels">labels()</a></code></li><li><code>Buckets = [{Bound, Count}]</code></li><li><code>Bound = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_buckets.html#type-bucket_bound">prometheus_buckets:bucket_bound()</a></code></li><li><code>Count = non_neg_integer()</code></li><li><code>Sum = <a href="#type-value">value()</a></code></li><li><code>Metric = <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a></code></li></ul>

Creates histogram metric with `Labels`, `Buckets`, `Count` and `Sum`.

<a name="histogram_metrics-1"></a>

### histogram_metrics/1 ###

`histogram_metrics(Specs) -> any()`

Equivalent to
[`lists:map(fun histogram_metric/1, Specs)`](#histogram_metric-1).

<a name="label_pair-1"></a>

### label_pair/1 ###

<pre><code>
label_pair(X1::<a href="#type-label">label()</a>) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-LabelPair">prometheus_model:'LabelPair'()</a>
</code></pre>
<br />

Creates `prometheus_model:`LabelPair'()' from {Name, Value} tuple.

<a name="label_pairs-1"></a>

### label_pairs/1 ###

`label_pairs(B) -> any()`

Equivalent to
[`lists:map(fun label_pair/1, Labels)`](#label_pair-1).

<a name="metric_name-1"></a>

### metric_name/1 ###

<pre><code>
metric_name(Name) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Name = atom() | binary() | [char() | iolist() | binary() | atom()]</code></li></ul>

If `Name` is a list, looks for atoms and converts them to binaries.
Why iolists do not support atoms?

<a name="summary_metric-1"></a>

### summary_metric/1 ###

<pre><code>
summary_metric(Summary) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Summary = <a href="#type-summary">summary()</a></code></li></ul>

Equivalent to
[`summary_metric(Labels, Count, Sum)`](#summary_metric-3).

<a name="summary_metric-2"></a>

### summary_metric/2 ###

`summary_metric(Count, Sum) -> any()`

Equivalent to [`summary_metric([], Count, Sum)`](#summary_metric-3).

<a name="summary_metric-3"></a>

### summary_metric/3 ###

`summary_metric(Labels, Count, Sum) -> any()`

Equivalent to [`summary_metric([], Count, Sum, [])`](#summary_metric-4).

<a name="summary_metric-4"></a>

### summary_metric/4 ###

<pre><code>
summary_metric(Labels, Count, Sum, Quantiles) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = <a href="#type-labels">labels()</a></code></li><li><code>Count = non_neg_integer()</code></li><li><code>Sum = <a href="#type-value">value()</a></code></li><li><code>Quantiles = list()</code></li></ul>

Creates summary metric with `Labels`, `Count` and `Sum`.

<a name="summary_metrics-1"></a>

### summary_metrics/1 ###

`summary_metrics(Specs) -> any()`

Equivalent to
[`lists:map(fun summary_metric/1, Specs)`](#summary_metric-1).

<a name="untyped_metric-1"></a>

### untyped_metric/1 ###

<pre><code>
untyped_metric(Untyped) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Untyped = <a href="#type-untyped">untyped()</a></code></li></ul>

Equivalent to
[`untyped_metric(Labels, Value)`](#untyped_metric-2).

<a name="untyped_metric-2"></a>

### untyped_metric/2 ###

<pre><code>
untyped_metric(Labels, Value) -&gt; <a href="http://www.erlang.org/edoc/doc/prometheus/doc/prometheus_model.html#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = <a href="#type-labels">labels()</a></code></li><li><code>Value = <a href="#type-value">value()</a></code></li></ul>

Creates untyped metric with `Labels` and `Value`.

<a name="untyped_metrics-1"></a>

### untyped_metrics/1 ###

`untyped_metrics(Values) -> any()`

Equivalent to
[`lists:map(fun untyped_metric/1, Values)`](#untyped_metric-1).

