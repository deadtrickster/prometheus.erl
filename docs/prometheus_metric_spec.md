

# Module prometheus_metric_spec #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-spec">spec()</a> ###


<pre><code>
spec() = <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fetch_value-2">fetch_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="fetch_value-2"></a>

### fetch_value/2 ###

<pre><code>
fetch_value(Key::atom(), Spec::<a href="#type-spec">spec()</a>) -&gt; any() | no_return()
</code></pre>
<br />

<a name="get_value-2"></a>

### get_value/2 ###

<pre><code>
get_value(Key::atom(), Spec::<a href="#type-spec">spec()</a>) -&gt; any()
</code></pre>
<br />

<a name="get_value-3"></a>

### get_value/3 ###

<pre><code>
get_value(Key::atom(), Spec::<a href="#type-spec">spec()</a>, Default::any()) -&gt; any()
</code></pre>
<br />

