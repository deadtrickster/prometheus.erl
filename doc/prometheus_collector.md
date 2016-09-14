

# Module prometheus_collector #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A collector for a set of metrics.

__This module defines the `prometheus_collector` behaviour.__<br /> Required callback functions: `collect_mf/2`, `collect_metrics/2`, `deregister_cleanup/1`.

<a name="description"></a>

## Description ##

Normal users should use [`prometheus_gauge`](prometheus_gauge.md),
[`prometheus_counter`](prometheus_counter.md), [`prometheus_summary`](prometheus_summary.md)
and [`prometheus_histogram`](prometheus_histogram.md).

Implementing `:prometheus_collector` behaviour is for advanced uses,
such as proxying metrics from another monitoring system.
It is it the responsibility of the implementer to ensure produced metrics
are valid.

You will be working with Prometheus
data model directly (see [`prometheus_model_helpers`](prometheus_model_helpers.md)).
<a name="types"></a>

## Data Types ##




### <a name="type-collect_mf_callback">collect_mf_callback()</a> ###


<pre><code>
collect_mf_callback() = fun((<a href="prometheus_model.md#type-MetricFamily">prometheus_model:'MetricFamily'()</a>) -&gt; any())
</code></pre>




### <a name="type-collector">collector()</a> ###


<pre><code>
collector() = atom()
</code></pre>




### <a name="type-data">data()</a> ###


<pre><code>
data() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_mf-3">collect_mf/3</a></td><td></td></tr><tr><td valign="top"><a href="#collect_mf_to_list-1">collect_mf_to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td>(<em>Deprecated</em>.) Equivalent to <a href="#deregister-2"><tt>deregister(Collector, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#deregister-2">deregister/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#enabled_collectors-0">enabled_collectors/0</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>(<em>Deprecated</em>.) Equivalent to <a href="#register-2"><tt>register(Collector, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td>(<em>Deprecated</em>.) </td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_mf-3"></a>

### collect_mf/3 ###

<pre><code>
collect_mf(Registry, Collector, Callback) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Callback = <a href="#type-collect_mf_callback">collect_mf_callback()</a></code></li></ul>

<a name="collect_mf_to_list-1"></a>

### collect_mf_to_list/1 ###

`collect_mf_to_list(Collector) -> any()`

<a name="deregister-1"></a>

### deregister/1 ###

`deregister(Collector) -> any()`

Equivalent to [`deregister(Collector, default)`](#deregister-2).

__This function is deprecated:__ Please use [`prometheus_registry:deregister_collector/1`](prometheus_registry.md#deregister_collector-1)

<a name="deregister-2"></a>

### deregister/2 ###

<pre><code>
deregister(Collector, Registry) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li></ul>

__This function is deprecated:__ Please use [`prometheus_registry:deregister_collector/2`](prometheus_registry.md#deregister_collector-2)

<a name="enabled_collectors-0"></a>

### enabled_collectors/0 ###

<pre><code>
enabled_collectors() -&gt; [<a href="#type-collector">collector()</a>]
</code></pre>
<br />

<a name="register-1"></a>

### register/1 ###

`register(Collector) -> any()`

Equivalent to [`register(Collector, default)`](#register-2).

__This function is deprecated:__ Please use [`prometheus_registry:register_collector/1`](prometheus_registry.md#register_collector-1)

<a name="register-2"></a>

### register/2 ###

<pre><code>
register(Collector, Registry) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li></ul>

__This function is deprecated:__ Please use [`prometheus_registry:register_collector/2`](prometheus_registry.md#register_collector-2)

