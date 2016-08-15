

# Module prometheus_buckets #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-bucket_bound">bucket_bound()</a> ###


<pre><code>
bucket_bound() = number() | infinity
</code></pre>




### <a name="type-buckets">buckets()</a> ###


<pre><code>
buckets() = [<a href="#type-bucket_bound">bucket_bound()</a>, ...]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default-0">default/0</a></td><td></td></tr><tr><td valign="top"><a href="#exponential-3">exponential/3</a></td><td></td></tr><tr><td valign="top"><a href="#generate_exponential-3">generate_exponential/3</a></td><td></td></tr><tr><td valign="top"><a href="#generate_linear-3">generate_linear/3</a></td><td></td></tr><tr><td valign="top"><a href="#linear-3">linear/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default-0"></a>

### default/0 ###

<pre><code>
default() -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

<a name="exponential-3"></a>

### exponential/3 ###

<pre><code>
exponential(Start::number(), Factor::number(), Count::pos_integer()) -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

<a name="generate_exponential-3"></a>

### generate_exponential/3 ###

`generate_exponential(Start, Factor, Count) -> any()`

<a name="generate_linear-3"></a>

### generate_linear/3 ###

`generate_linear(Start, Step, Count) -> any()`

<a name="linear-3"></a>

### linear/3 ###

<pre><code>
linear(Start::integer(), Step::pos_integer(), Count::pos_integer()) -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

