

# Module prometheus_model_helpers #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#counter_metric-1">counter_metric/1</a></td><td></td></tr><tr><td valign="top"><a href="#counter_metric-2">counter_metric/2</a></td><td></td></tr><tr><td valign="top"><a href="#counter_metrics-1">counter_metrics/1</a></td><td>Equivalent to
<a href="#counter_metric-1"><code>lists:map(fun counter_metric/1, Specs)</code></a>.</td></tr><tr><td valign="top"><a href="#create_mf-5">create_mf/5</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_metric-1">gauge_metric/1</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_metric-2">gauge_metric/2</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_metrics-1">gauge_metrics/1</a></td><td>Equivalent to
<a href="#gauge_metric-1"><code>lists:map(fun gauge_metric/1, Values)</code></a>.</td></tr><tr><td valign="top"><a href="#histogram_metric-1">histogram_metric/1</a></td><td></td></tr><tr><td valign="top"><a href="#histogram_metric-4">histogram_metric/4</a></td><td></td></tr><tr><td valign="top"><a href="#histogram_metrics-1">histogram_metrics/1</a></td><td>Equivalent to
<a href="#histogram_metric-1"><code>lists:map(fun histogram_metric/1, Specs)</code></a>.</td></tr><tr><td valign="top"><a href="#label_pair-1">label_pair/1</a></td><td></td></tr><tr><td valign="top"><a href="#label_pairs-1">label_pairs/1</a></td><td>Equivalent to
<a href="#label_pair-1"><code>lists:map(fun label_pair/1, Labels)</code></a>.</td></tr><tr><td valign="top"><a href="#summary_metric-1">summary_metric/1</a></td><td></td></tr><tr><td valign="top"><a href="#summary_metric-2">summary_metric/2</a></td><td>Equivalent to <a href="#summary_metric-3"><tt>summary_metric([], Count, Sum)</tt></a>.</td></tr><tr><td valign="top"><a href="#summary_metric-3">summary_metric/3</a></td><td></td></tr><tr><td valign="top"><a href="#summary_metrics-1">summary_metrics/1</a></td><td>Equivalent to
<a href="#summary_metric-1"><code>lists:map(fun summary_metric/1, Specs)</code></a>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="counter_metric-1"></a>

### counter_metric/1 ###

<pre><code>
counter_metric(Value) -&gt; <a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Value = {Labels, Val} | {Val} | Val</code></li><li><code>Labels = [<a href="#type-label">label()</a>]</code></li><li><code>Val = non_neg_integer()</code></li></ul>

<a name="counter_metric-2"></a>

### counter_metric/2 ###

<pre><code>
counter_metric(Labels, Value) -&gt; <a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = [<a href="#type-label">label()</a>]</code></li><li><code>Value = non_neg_integer()</code></li></ul>

<a name="counter_metrics-1"></a>

### counter_metrics/1 ###

`counter_metrics(Specs) -> any()`

Equivalent to
[`lists:map(fun counter_metric/1, Specs)`](#counter_metric-1).

<a name="create_mf-5"></a>

### create_mf/5 ###

<pre><code>
create_mf(Name, Help, Type, Collector, CollectorData) -&gt; MetricFamily
</code></pre>

<ul class="definitions"><li><code>Name = <a href="prometheus_metric.md#type-name">prometheus_metric:name()</a></code></li><li><code>Help = <a href="prometheus_metric.md#type-help">prometheus_metric:help()</a></code></li><li><code>Type = atom()</code></li><li><code>Collector = <a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a></code></li><li><code>CollectorData = <a href="prometheus_collector.md#type-data">prometheus_collector:data()</a></code></li><li><code>MetricFamily = <a href="prometheus_model.md#type-MetricFamily">prometheus_model:'MetricFamily'()</a></code></li></ul>

<a name="gauge_metric-1"></a>

### gauge_metric/1 ###

<pre><code>
gauge_metric(Value) -&gt; <a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Value = integer()</code></li></ul>

<a name="gauge_metric-2"></a>

### gauge_metric/2 ###

<pre><code>
gauge_metric(Labels, Value) -&gt; <a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = [<a href="#type-label">label()</a>]</code></li><li><code>Value = non_neg_integer()</code></li></ul>

<a name="gauge_metrics-1"></a>

### gauge_metrics/1 ###

`gauge_metrics(Values) -> any()`

Equivalent to
[`lists:map(fun gauge_metric/1, Values)`](#gauge_metric-1).

<a name="histogram_metric-1"></a>

### histogram_metric/1 ###

`histogram_metric(X1) -> any()`

<a name="histogram_metric-4"></a>

### histogram_metric/4 ###

<pre><code>
histogram_metric(Labels, Buckets, Count, Sum) -&gt; Metric
</code></pre>

<ul class="definitions"><li><code>Labels = [<a href="#type-label">label()</a>]</code></li><li><code>Buckets = [{Bound, Count}]</code></li><li><code>Bound = <a href="prometheus_buckets.md#type-bucket_bound">prometheus_buckets:bucket_bound()</a></code></li><li><code>Count = non_neg_integer()</code></li><li><code>Sum = non_neg_integer()</code></li><li><code>Metric = <a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a></code></li></ul>

<a name="histogram_metrics-1"></a>

### histogram_metrics/1 ###

`histogram_metrics(Specs) -> any()`

Equivalent to
[`lists:map(fun histogram_metric/1, Specs)`](#histogram_metric-1).

<a name="label_pair-1"></a>

### label_pair/1 ###

<pre><code>
label_pair(X1::<a href="#type-label">label()</a>) -&gt; <a href="prometheus_model.md#type-LabelPair">prometheus_model:'LabelPair'()</a>
</code></pre>
<br />

<a name="label_pairs-1"></a>

### label_pairs/1 ###

`label_pairs(Labels) -> any()`

Equivalent to
[`lists:map(fun label_pair/1, Labels)`](#label_pair-1).

<a name="summary_metric-1"></a>

### summary_metric/1 ###

<pre><code>
summary_metric(Spec) -&gt; <a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Spec = {Labels, Count, Sum} | {Count, Sum}</code></li><li><code>Labels = [<a href="#type-label">label()</a>]</code></li><li><code>Count = non_neg_integer()</code></li><li><code>Sum = non_neg_integer()</code></li></ul>

<a name="summary_metric-2"></a>

### summary_metric/2 ###

`summary_metric(Count, Sum) -> any()`

Equivalent to [`summary_metric([], Count, Sum)`](#summary_metric-3).

<a name="summary_metric-3"></a>

### summary_metric/3 ###

<pre><code>
summary_metric(Labels, Count, Sum) -&gt; <a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a>
</code></pre>

<ul class="definitions"><li><code>Labels = [<a href="#type-label">label()</a>]</code></li><li><code>Count = non_neg_integer()</code></li><li><code>Sum = non_neg_integer()</code></li></ul>

<a name="summary_metrics-1"></a>

### summary_metrics/1 ###

`summary_metrics(Specs) -> any()`

Equivalent to
[`lists:map(fun summary_metric/1, Specs)`](#summary_metric-1).

