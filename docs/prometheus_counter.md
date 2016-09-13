

# Module prometheus_counter #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Counter is a Metric that represents a single numerical value that only ever
goes up.

__Behaviours:__ [`gen_server`](gen_server.md), [`prometheus_collector`](prometheus_collector.md), [`prometheus_metric`](prometheus_metric.md).

<a name="description"></a>

## Description ##

That implies that it cannot be used to count items whose number can
also go down, e.g. the number of currently running processes. Those
"counters" are represented by [`prometheus_gauge`](prometheus_gauge.md).

A Counter is typically used to count requests served, tasks completed, errors
occurred, etc.

Examople use cases for Counters:

* Number of requests processed

* Number of items that were inserted into a queue

* Total amount of data a system has processed


Use the
[rate()](https://prometheus.io/docs/querying/functions/#rate())/[irate()](https://prometheus.io/docs/querying/functions/#irate())
functions in Prometheus to calculate the rate of increase of a Counter.
By convention, the names of Counters are suffixed by `_total`.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td></td></tr><tr><td valign="top"><a href="#declare-2">declare/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#dinc-1">dinc/1</a></td><td>Equivalent to <a href="#dinc-4"><tt>dinc(default, Name, [], 1)</tt></a>.</td></tr><tr><td valign="top"><a href="#dinc-2">dinc/2</a></td><td></td></tr><tr><td valign="top"><a href="#dinc-3">dinc/3</a></td><td>Equivalent to <a href="#dinc-4"><tt>dinc(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dinc-4">dinc/4</a></td><td></td></tr><tr><td valign="top"><a href="#inc-1">inc/1</a></td><td>Equivalent to <a href="#inc-4"><tt>inc(default, Name, [], 1)</tt></a>.</td></tr><tr><td valign="top"><a href="#inc-2">inc/2</a></td><td></td></tr><tr><td valign="top"><a href="#inc-3">inc/3</a></td><td>Equivalent to <a href="#inc-4"><tt>inc(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#inc-4">inc/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

<a name="declare-2"></a>

### declare/2 ###

`declare(Spec, Registry) -> any()`

__This function is deprecated:__ Please use [`declare/1`](#declare-1) with registry
key instead.

<a name="dinc-1"></a>

### dinc/1 ###

`dinc(Name) -> any()`

Equivalent to [`dinc(default, Name, [], 1)`](#dinc-4).

<a name="dinc-2"></a>

### dinc/2 ###

`dinc(Name, LabelValues) -> any()`

<a name="dinc-3"></a>

### dinc/3 ###

`dinc(Name, LabelValues, Value) -> any()`

Equivalent to [`dinc(default, Name, LabelValues, Value)`](#dinc-4).

<a name="dinc-4"></a>

### dinc/4 ###

`dinc(Registry, Name, LabelValues, Value) -> any()`

<a name="inc-1"></a>

### inc/1 ###

`inc(Name) -> any()`

Equivalent to [`inc(default, Name, [], 1)`](#inc-4).

<a name="inc-2"></a>

### inc/2 ###

`inc(Name, LabelValues) -> any()`

<a name="inc-3"></a>

### inc/3 ###

`inc(Name, LabelValues, Value) -> any()`

Equivalent to [`inc(default, Name, LabelValues, Value)`](#inc-4).

<a name="inc-4"></a>

### inc/4 ###

`inc(Registry, Name, LabelValues, Value) -> any()`

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

<a name="new-2"></a>

### new/2 ###

`new(Spec, Registry) -> any()`

__This function is deprecated:__ Please use [`new/1`](#new-1) with registry
key instead.

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

