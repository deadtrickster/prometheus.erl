

# Module prometheus_instrumenter #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `prometheus_instrumenter` behaviour.__<br /> Required callback functions: `setup_instrumenter/0`.

<a name="types"></a>

## Data Types ##




### <a name="type-instrumenter">instrumenter()</a> ###


<pre><code>
instrumenter() = atom()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#enabled_instrumenters-0">enabled_instrumenters/0</a></td><td></td></tr><tr><td valign="top"><a href="#setup-1">setup/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="enabled_instrumenters-0"></a>

### enabled_instrumenters/0 ###

<pre><code>
enabled_instrumenters() -&gt; [<a href="#type-instrumenter">instrumenter()</a>]
</code></pre>
<br />

<a name="setup-1"></a>

### setup/1 ###

<pre><code>
setup(Instrumenter) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Instrumenter = <a href="#type-instrumenter">instrumenter()</a></code></li><li><code>Result = ok</code></li></ul>

