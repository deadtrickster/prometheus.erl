

# Module prometheus_histogram #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A Histogram tracks the size and number of events in buckets.

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md), [`prometheus_metric`](prometheus_metric.md).

<a name="description"></a>

## Description ##

You can use Histograms for aggregatable calculation of quantiles.

Example use cases for Histograms:

* Response latency

* Request size

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#buckets-1">buckets/1</a></td><td>Equivalent to <a href="#buckets-3"><tt>buckets(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#buckets-2">buckets/2</a></td><td>Equivalent to <a href="#buckets-3"><tt>buckets(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#buckets-3">buckets/3</a></td><td></td></tr><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td></td></tr><tr><td valign="top"><a href="#declare-2">declare/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#default_buckets-0">default_buckets/0</a></td><td></td></tr><tr><td valign="top"><a href="#dobserve-2">dobserve/2</a></td><td>Equivalent to <a href="#dobserve-4"><tt>dobserve(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dobserve-3">dobserve/3</a></td><td>Equivalent to <a href="#dobserve-5"><tt>dobserve(default, Name, LabelValues, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dobserve-4">dobserve/4</a></td><td></td></tr><tr><td valign="top"><a href="#exponential_buckets-3">exponential_buckets/3</a></td><td></td></tr><tr><td valign="top"><a href="#linear_buckets-3">linear_buckets/3</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#observe-2">observe/2</a></td><td>Equivalent to <a href="#observe-4"><tt>observe(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe-3">observe/3</a></td><td>Equivalent to <a href="#observe-4"><tt>observe(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe-4">observe/4</a></td><td></td></tr><tr><td valign="top"><a href="#observe_duration-2">observe_duration/2</a></td><td>Equivalent to <a href="#observe_duration-4"><tt>observe_duration(default, Name, [], Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe_duration-3">observe_duration/3</a></td><td>Equivalent to <a href="#observe_duration-4"><tt>observe_duration(default, Name, LabelValues, Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe_duration-4">observe_duration/4</a></td><td></td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="buckets-1"></a>

### buckets/1 ###

`buckets(Name) -> any()`

Equivalent to [`buckets(default, Name, [])`](#buckets-3).

<a name="buckets-2"></a>

### buckets/2 ###

`buckets(Name, LabelValues) -> any()`

Equivalent to [`buckets(default, Name, LabelValues)`](#buckets-3).

<a name="buckets-3"></a>

### buckets/3 ###

`buckets(Registry, Name, LabelValues) -> any()`

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

<a name="declare-2"></a>

### declare/2 ###

`declare(Spec, Registry) -> any()`

__This function is deprecated:__ Please use [`declare/1`](#declare-1) with registry
key instead.

<a name="default_buckets-0"></a>

### default_buckets/0 ###

`default_buckets() -> any()`

<a name="dobserve-2"></a>

### dobserve/2 ###

`dobserve(Name, Value) -> any()`

Equivalent to [`dobserve(default, Name, [], Value)`](#dobserve-4).

<a name="dobserve-3"></a>

### dobserve/3 ###

`dobserve(Name, LabelValues, Value) -> any()`

Equivalent to [`dobserve(default, Name, LabelValues, [], Value)`](#dobserve-5).

<a name="dobserve-4"></a>

### dobserve/4 ###

`dobserve(Registry, Name, LabelValues, Value) -> any()`

<a name="exponential_buckets-3"></a>

### exponential_buckets/3 ###

`exponential_buckets(Start, Factor, Count) -> any()`

<a name="linear_buckets-3"></a>

### linear_buckets/3 ###

`linear_buckets(Start, Step, Count) -> any()`

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

<a name="new-2"></a>

### new/2 ###

`new(Spec, Registry) -> any()`

__This function is deprecated:__ Please use [`new/1`](#new-1) with registry
key instead.

<a name="observe-2"></a>

### observe/2 ###

`observe(Name, Value) -> any()`

Equivalent to [`observe(default, Name, [], Value)`](#observe-4).

<a name="observe-3"></a>

### observe/3 ###

`observe(Name, LabelValues, Value) -> any()`

Equivalent to [`observe(default, Name, LabelValues, Value)`](#observe-4).

<a name="observe-4"></a>

### observe/4 ###

`observe(Registry, Name, LabelValues, Value) -> any()`

<a name="observe_duration-2"></a>

### observe_duration/2 ###

`observe_duration(Name, Fun) -> any()`

Equivalent to [`observe_duration(default, Name, [], Fun)`](#observe_duration-4).

<a name="observe_duration-3"></a>

### observe_duration/3 ###

`observe_duration(Name, LabelValues, Fun) -> any()`

Equivalent to [`observe_duration(default, Name, LabelValues, Fun)`](#observe_duration-4).

<a name="observe_duration-4"></a>

### observe_duration/4 ###

`observe_duration(Registry, Name, LabelValues, Fun) -> any()`

<a name="remove-1"></a>

### remove/1 ###

`remove(Name) -> any()`

Equivalent to [`remove(default, Name, [])`](#remove-3).

<a name="remove-2"></a>

### remove/2 ###

`remove(Name, LabelValues) -> any()`

Equivalent to [`remove(default, Name, LabelValues)`](#remove-3).

<a name="remove-3"></a>

### remove/3 ###

`remove(Registry, Name, LabelValues) -> any()`

<a name="reset-1"></a>

### reset/1 ###

`reset(Name) -> any()`

Equivalent to [`reset(default, Name, [])`](#reset-3).

<a name="reset-2"></a>

### reset/2 ###

`reset(Name, LabelValues) -> any()`

Equivalent to [`reset(default, Name, LabelValues)`](#reset-3).

<a name="reset-3"></a>

### reset/3 ###

`reset(Registry, Name, LabelValues) -> any()`

<a name="value-1"></a>

### value/1 ###

`value(Name) -> any()`

Equivalent to [`value(default, Name, [])`](#value-3).

<a name="value-2"></a>

### value/2 ###

`value(Name, LabelValues) -> any()`

Equivalent to [`value(default, Name, LabelValues)`](#value-3).

<a name="value-3"></a>

### value/3 ###

`value(Registry, Name, LabelValues) -> any()`

