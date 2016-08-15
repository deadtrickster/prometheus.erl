

# Module prometheus_text_format #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`prometheus_format`](prometheus_format.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#content_type-0">content_type/0</a></td><td></td></tr><tr><td valign="top"><a href="#escape_label_value-1">escape_label_value/1</a></td><td></td></tr><tr><td valign="top"><a href="#escape_metric_help-1">escape_metric_help/1</a></td><td></td></tr><tr><td valign="top"><a href="#format-0">format/0</a></td><td>Equivalent to <a href="#format-1"><tt>format(default)</tt></a>.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="content_type-0"></a>

### content_type/0 ###

<pre><code>
content_type() -&gt; binary()
</code></pre>
<br />

<a name="escape_label_value-1"></a>

### escape_label_value/1 ###

<pre><code>
escape_label_value(LValue::binary() | iolist() | undefined) -&gt; string()
</code></pre>
<br />

<a name="escape_metric_help-1"></a>

### escape_metric_help/1 ###

`escape_metric_help(Help) -> any()`

<a name="format-0"></a>

### format/0 ###

<pre><code>
format() -&gt; binary()
</code></pre>
<br />

Equivalent to [`format(default)`](#format-1).

<a name="format-1"></a>

### format/1 ###

<pre><code>
format(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>) -&gt; binary()
</code></pre>
<br />

