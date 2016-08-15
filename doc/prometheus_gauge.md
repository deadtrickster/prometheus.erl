

# Module prometheus_gauge #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md), [`prometheus_metric`](prometheus_metric.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_metrics-2">collect_metrics/2</a></td><td></td></tr><tr><td valign="top"><a href="#collect_mf-2">collect_mf/2</a></td><td></td></tr><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td>Equivalent to <a href="#declare-2"><tt>declare(Spec, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#declare-2">declare/2</a></td><td></td></tr><tr><td valign="top"><a href="#deregister_cleanup-1">deregister_cleanup/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Spec, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td></td></tr><tr><td valign="top"><a href="#set_to_current_time-1">set_to_current_time/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_to_current_time-2">set_to_current_time/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_to_current_time-3">set_to_current_time/3</a></td><td></td></tr><tr><td valign="top"><a href="#track_inprogress-2">track_inprogress/2</a></td><td></td></tr><tr><td valign="top"><a href="#track_inprogress-3">track_inprogress/3</a></td><td></td></tr><tr><td valign="top"><a href="#track_inprogress-4">track_inprogress/4</a></td><td></td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_metrics-2"></a>

### collect_metrics/2 ###

`collect_metrics(Name, X2) -> any()`

<a name="collect_mf-2"></a>

### collect_mf/2 ###

`collect_mf(Callback, Registry) -> any()`

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

Equivalent to [`declare(Spec, default)`](#declare-2).

<a name="declare-2"></a>

### declare/2 ###

`declare(Spec, Registry) -> any()`

<a name="deregister_cleanup-1"></a>

### deregister_cleanup/1 ###

`deregister_cleanup(Registry) -> any()`

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

Equivalent to [`new(Spec, default)`](#new-2).

<a name="new-2"></a>

### new/2 ###

`new(Spec, Registry) -> any()`

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

<a name="set-2"></a>

### set/2 ###

`set(Name, Value) -> any()`

<a name="set-3"></a>

### set/3 ###

`set(Name, LabelValues, Value) -> any()`

<a name="set-4"></a>

### set/4 ###

`set(Registry, Name, LabelValues, Value) -> any()`

<a name="set_to_current_time-1"></a>

### set_to_current_time/1 ###

`set_to_current_time(Name) -> any()`

<a name="set_to_current_time-2"></a>

### set_to_current_time/2 ###

`set_to_current_time(Name, LabelValues) -> any()`

<a name="set_to_current_time-3"></a>

### set_to_current_time/3 ###

`set_to_current_time(Registry, Name, LabelValues) -> any()`

<a name="track_inprogress-2"></a>

### track_inprogress/2 ###

`track_inprogress(Name, Fun) -> any()`

<a name="track_inprogress-3"></a>

### track_inprogress/3 ###

`track_inprogress(Name, LabelValues, Fun) -> any()`

<a name="track_inprogress-4"></a>

### track_inprogress/4 ###

`track_inprogress(Registry, Name, LabelValues, Fun) -> any()`

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

