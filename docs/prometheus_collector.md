

# Module prometheus_collector #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `prometheus_collector` behaviour.__<br /> Required callback functions: `collect_mf/2`, `collect_metrics/2`, `deregister_cleanup/1`.

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_mf-3">collect_mf/3</a></td><td></td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td>Equivalent to <a href="#deregister-2"><tt>deregister(Collector, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#deregister-2">deregister/2</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>Equivalent to <a href="#register-2"><tt>register(Collector, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_mf-3"></a>

### collect_mf/3 ###

<pre><code>
collect_mf(Collector, Callback, Registry) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Callback = <a href="#type-collect_mf_callback">collect_mf_callback()</a></code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li></ul>

<a name="deregister-1"></a>

### deregister/1 ###

`deregister(Collector) -> any()`

Equivalent to [`deregister(Collector, default)`](#deregister-2).

<a name="deregister-2"></a>

### deregister/2 ###

<pre><code>
deregister(Collector, Registry) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li></ul>

<a name="register-1"></a>

### register/1 ###

`register(Collector) -> any()`

Equivalent to [`register(Collector, default)`](#register-2).

<a name="register-2"></a>

### register/2 ###

<pre><code>
register(Collector, Registry) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li></ul>

