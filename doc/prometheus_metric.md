

# Module prometheus_metric #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `prometheus_metric` behaviour.__<br /> Required callback functions: `new/1`, `declare/1`, `remove/1`, `remove/2`, `remove/3`, `reset/1`, `reset/2`, `reset/3`, `value/1`, `value/2`, `value/3`.

<a name="types"></a>

## Data Types ##




### <a name="type-call_enabled">call_enabled()</a> ###


<pre><code>
call_enabled() = boolean()
</code></pre>




### <a name="type-counter_value">counter_value()</a> ###


<pre><code>
counter_value() = number()
</code></pre>




### <a name="type-duration_unit">duration_unit()</a> ###


<pre><code>
duration_unit() = microseconds | milliseconds | seconds | minutes | days
</code></pre>




### <a name="type-gauge_value">gauge_value()</a> ###


<pre><code>
gauge_value() = number()
</code></pre>




### <a name="type-help">help()</a> ###


<pre><code>
help() = binary() | nonempty_string()
</code></pre>




### <a name="type-histogram_value">histogram_value()</a> ###


<pre><code>
histogram_value() = {Buckets::[number(), ...], Sum::number()}
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = atom() | binary() | nonempty_string()
</code></pre>




### <a name="type-summary_value">summary_value()</a> ###


<pre><code>
summary_value() = {Count::number(), Sum::number()}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = <a href="#type-counter_value">counter_value()</a> | <a href="#type-gauge_value">gauge_value()</a> | <a href="#type-summary_value">summary_value()</a> | <a href="#type-histogram_value">histogram_value()</a> | undefined
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#mf_call_timeout-1">mf_call_timeout/1</a></td><td></td></tr><tr><td valign="top"><a href="#mf_duration_unit-1">mf_duration_unit/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_labels-4">remove_labels/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="mf_call_timeout-1"></a>

### mf_call_timeout/1 ###

`mf_call_timeout(MF) -> any()`

<a name="mf_duration_unit-1"></a>

### mf_duration_unit/1 ###

`mf_duration_unit(MF) -> any()`

<a name="remove_labels-4"></a>

### remove_labels/4 ###

<pre><code>
remove_labels(Table, Registry, Name, LValues) -&gt; boolean() | no_return()
</code></pre>

<ul class="definitions"><li><code>Table = atom()</code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li><li><code>Name = <a href="#type-name">name()</a></code></li><li><code>LValues = list()</code></li></ul>

