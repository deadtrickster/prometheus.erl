

# Module prometheus_http #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

HTTP instrumentation helpers.

<a name="types"></a>

## Data Types ##




### <a name="type-status_class">status_class()</a> ###


<pre><code>
status_class() = <a href="prometheus_model_helpers.md#type-label_value">prometheus_model_helpers:label_value()</a>
</code></pre>




### <a name="type-status_code">status_code()</a> ###


<pre><code>
status_code() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#microseconds_duration_buckets-0">microseconds_duration_buckets/0</a></td><td>default microseconds buckets for measuring http requests duration.</td></tr><tr><td valign="top"><a href="#status_class-1">status_class/1</a></td><td>returns status class for http status code.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="microseconds_duration_buckets-0"></a>

### microseconds_duration_buckets/0 ###

<pre><code>
microseconds_duration_buckets() -&gt; <a href="prometheus_buckets.md#type-buckets">prometheus_buckets:buckets()</a>
</code></pre>
<br />

default microseconds buckets for measuring http requests duration

<a name="status_class-1"></a>

### status_class/1 ###

<pre><code>
status_class(SCode) -&gt; StatusClass
</code></pre>

<ul class="definitions"><li><code>SCode = <a href="#type-status_code">status_code()</a></code></li><li><code>StatusClass = <a href="#type-status_class">status_class()</a></code></li></ul>

returns status class for http status code

