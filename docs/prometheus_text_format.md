

# Module prometheus_text_format #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Serializes Prometheus registry using the latest text format.

__Behaviours:__ [`prometheus_format`](prometheus_format.md).

<a name="description"></a>

## Description ##
Example output:

```

    # TYPE http_request_duration_milliseconds histogram
    # HELP http_request_duration_milliseconds Http Request execution time
    http_request_duration_milliseconds_bucket{method="post",le="100"} 0
    http_request_duration_milliseconds_bucket{method="post",le="300"} 1
    http_request_duration_milliseconds_bucket{method="post",le="500"} 3
    http_request_duration_milliseconds_bucket{method="post",le="750"} 4
    http_request_duration_milliseconds_bucket{method="post",le="1000"} 5
    http_request_duration_milliseconds_bucket{method="post",le="+Inf"} 6
    http_request_duration_milliseconds_count{method="post"} 6
    http_request_duration_milliseconds_sum{method="post"} 4350
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#content_type-0">content_type/0</a></td><td>
Content type of the latest text format.</td></tr><tr><td valign="top"><a href="#emit_mf_metrics-2">emit_mf_metrics/2</a></td><td></td></tr><tr><td valign="top"><a href="#emit_mf_prologue-2">emit_mf_prologue/2</a></td><td></td></tr><tr><td valign="top"><a href="#format-0">format/0</a></td><td>
Format <code>default</code> registry using the latest text format.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td>
Format <code>registry</code> using the latest text format.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="content_type-0"></a>

### content_type/0 ###

<pre><code>
content_type() -&gt; binary()
</code></pre>
<br />

Content type of the latest text format.

<a name="emit_mf_metrics-2"></a>

### emit_mf_metrics/2 ###

`emit_mf_metrics(Fd, MetricFamily) -> any()`

<a name="emit_mf_prologue-2"></a>

### emit_mf_prologue/2 ###

`emit_mf_prologue(Fd, MetricFamily) -> any()`

<a name="format-0"></a>

### format/0 ###

<pre><code>
format() -&gt; binary()
</code></pre>
<br />

Equivalent to [`format(default)`](#format-1).

Format `default` registry using the latest text format.

<a name="format-1"></a>

### format/1 ###

<pre><code>
format(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>) -&gt; binary()
</code></pre>
<br />

Format `registry` using the latest text format.

