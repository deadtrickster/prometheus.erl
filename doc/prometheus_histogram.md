

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


Histogram expects `buckets` key in a metric spec. Buckets can be:
- a list of numbers in increasing order;
- :default;
- {:linear, start, step, count};
- {:exponential, start, step, count}

Example:

```erlang

  -module(example_instrumenter).
  setup() ->
    prometheus_histogram:declare([{name, http_request_duration_milliseconds},
                                  {labels, [method]},
                                  {buckets, [100, 300, 500, 750, 1000]},
                                  {help, "Http Request execution time."}]).
  instrument(Time, Method) ->
    %% Time must be in native units, otherwise duration_unit must be false
    prometheus_histogram:observe(http_request_duration_milliseconds,
                                 [Method], Time).
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#buckets-1">buckets/1</a></td><td>Equivalent to <a href="#buckets-3"><tt>buckets(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#buckets-2">buckets/2</a></td><td>Equivalent to <a href="#buckets-3"><tt>buckets(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#buckets-3">buckets/3</a></td><td>Returns buckets of the histogram identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td>Creates a histogram using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td>Equivalent to <a href="#deregister-2"><tt>deregister(default, Name)</tt></a>.</td></tr><tr><td valign="top"><a href="#deregister-2">deregister/2</a></td><td>
Removes all histogram series with name <code>Name</code> and
removes Metric Family from <code>Registry</code>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a histogram using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#observe-2">observe/2</a></td><td>Equivalent to <a href="#observe-4"><tt>observe(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe-3">observe/3</a></td><td>Equivalent to <a href="#observe-4"><tt>observe(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe-4">observe/4</a></td><td>Observes the given <code>Value</code>.</td></tr><tr><td valign="top"><a href="#observe_duration-2">observe_duration/2</a></td><td>Equivalent to <a href="#observe_duration-4"><tt>observe_duration(default, Name, [], Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe_duration-3">observe_duration/3</a></td><td>Equivalent to <a href="#observe_duration-4"><tt>observe_duration(default, Name, LabelValues, Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe_duration-4">observe_duration/4</a></td><td>Tracks the amount of time spent executing <code>Fun</code>.</td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td>Removes histogram series identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td>Resets the value of the histogram identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td>Returns the value of the histogram identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#values-2">values/2</a></td><td></td></tr></table>


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

Returns buckets of the histogram identified by `Registry`, `Name`
and `LabelValues`.

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

Creates a histogram using `Spec`.
If a histogram with the same `Spec` exists returns `false`.

Raises `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key
is missing.<br />
Raises `{invalid_metric_name, Name, Message}` error if metric `Name`
is invalid.<br />
Raises `{invalid_metric_help, Help, Message}` error if metric `Help`
is invalid.<br />
Raises `{invalid_metric_labels, Labels, Message}` error if `Labels`
isn't a list.<br />
Raises `{invalid_label_name, Name, Message}` error if `Name` isn't a valid
label name.<br />
Raises `{invalid_value_error, Value, MessagE}` error if `duration_unit` is
unknown or doesn't match metric name.<br />

Histogram-specific errors:<br />
Raises `{no_buckets, Buckets}` error if `Buckets` are missing,
not a list, empty list or not known buckets spec.<br />
Raises `{invalid_buckets, Buckets, Message}` error if `Buckets`
aren't in increasing order.<br />
Raises `{invalid_bound, Bound}` error if `Bound` isn't a number.

<a name="deregister-1"></a>

### deregister/1 ###

`deregister(Name) -> any()`

Equivalent to [`deregister(default, Name)`](#deregister-2).

<a name="deregister-2"></a>

### deregister/2 ###

`deregister(Registry, Name) -> any()`

Removes all histogram series with name `Name` and
removes Metric Family from `Registry`.

After this call new/1 for `Name` and `Registry` will succeed.

Returns `{true, _}` if `Name` was a registered histogram.
Otherwise returns `{false, _}`.

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

Creates a histogram using `Spec`.

Raises `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key
is missing.<br />
Raises `{invalid_metric_name, Name, Message}` error if metric `Name`
is invalid.<br />
Raises `{invalid_metric_help, Help, Message}` error if metric `Help`
is invalid.<br />
Raises `{invalid_metric_labels, Labels, Message}` error if `Labels`
isn't a list.<br />
Raises `{invalid_label_name, Name, Message}` error if `Name` isn't a valid
label name.<br />
Raises `{invalid_value_error, Value, Message}` error if `duration_unit` is
unknown or doesn't match metric name.<br />
Raises `{mf_already_exists, {Registry, Name}, Message}` error if a histogram
with the same `Spec` already exists.

Histogram-specific errors:<br />
Raises `{no_buckets, Buckets}` error if `Buckets` are missing,
not a list, empty list or not known buckets spec.<br />
Raises `{invalid_buckets, Buckets, Message}` error if `Buckets`
aren't in increasing order.<br />
Raises `{invalid_bound, Bound}` error if `Bound` isn't a number.

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

Observes the given `Value`.

Raises `{invalid_value, Value, Message}` if `Value`
isn't an integer.<br />
Raises `{unknown_metric, Registry, Name}` error if histogram with named
`Name` can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

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

Tracks the amount of time spent executing `Fun`.

Raises `{unknown_metric, Registry, Name}` error if histogram with named
`Name` can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.
Raises `{invalid_value, Value, Message}` if `Fun`
isn't a function.<br />

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

Removes histogram series identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if histogram with name
`Name` can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

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

Resets the value of the histogram identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if histogram with name
`Name` can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

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

Returns the value of the histogram identified by `Registry`, `Name`
and `LabelValues`. If there is no histogram for `LabelValues`,
returns `undefined`.

If duration unit set, sum will be converted to the duration unit.
[Read more here.](prometheus_time.md)

Raises `{unknown_metric, Registry, Name}` error if histogram named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="values-2"></a>

### values/2 ###

`values(Registry, Name) -> any()`

