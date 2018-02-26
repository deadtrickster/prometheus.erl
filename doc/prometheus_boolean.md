

# Module prometheus_boolean #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Boolean metric, to report booleans and flags.

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md), [`prometheus_metric`](prometheus_metric.md).

<a name="description"></a>

## Description ##

Boolean is a non-standard metric that uses untyped metric underneath.

A Boolean is typically used as a flag i.e. enabled/disabled, online/offline.

Example:

```erlang

  -module(my_fuse_instrumenter).
  -export([setup/0,
           fuse_event/2]).
  setup() ->
    prometheus_boolean:declare([{name, app_fuse_state},
                                {labels, [name]}, %% fuse name
                                {help, "State of various app fuses."}]),
  fuse_event(Fuse, Event) ->
    case Event of
      ok -> prometheus_boolean:set(app_fuse_state, [Fuse], true);
      blown -> prometheus_boolean:set(app_fuse_state, [Fuse], false);
      _ -> ok
    end.
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td>Creates a boolean using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td>Equivalent to <a href="#deregister-2"><tt>deregister(default, Name)</tt></a>.</td></tr><tr><td valign="top"><a href="#deregister-2">deregister/2</a></td><td>
Removes all boolean series with name <code>Name</code> and
removes Metric Family from <code>Registry</code>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a boolean using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td>Removes boolean series identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td>Resets the value of the boolean identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Equivalent to <a href="#set-4"><tt>set(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Equivalent to <a href="#set-4"><tt>set(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td>Sets the boolean identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code> to <code>Value</code>.</td></tr><tr><td valign="top"><a href="#toggle-1">toggle/1</a></td><td>Equivalent to <a href="#toggle-3"><tt>toggle(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#toggle-2">toggle/2</a></td><td>Equivalent to <a href="#toggle-4"><tt>toggle(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#toggle-3">toggle/3</a></td><td>Toggles the boolean identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td>Returns the value of the boolean identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#values-2">values/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

Creates a boolean using `Spec`.
If a boolean with the same `Spec` exists returns `false`.

Raises `{missing_metric_spec_key, Key, Spec}` error if required `Soec` key
is missing.<br />
Raises `{invalid_metric_name, Name, Message}` error if metric `Name`
is invalid.<br />
Raises `{invalid_metric_help, Help, Message}` error if metric `Help`
is invalid.<br />
Raises `{invalid_metric_labels, Labels, Message}` error if `Labels`
isn't a list.<br />
Raises `{invalid_label_name, Name, Message}` error if `Name` isn't a valid
label name.<br />

<a name="deregister-1"></a>

### deregister/1 ###

`deregister(Name) -> any()`

Equivalent to [`deregister(default, Name)`](#deregister-2).

<a name="deregister-2"></a>

### deregister/2 ###

`deregister(Registry, Name) -> any()`

Removes all boolean series with name `Name` and
removes Metric Family from `Registry`.

After this call new/1 for `Name` and `Registry` will succeed.

Returns `{true, _}` if `Name` was a registered boolean.
Otherwise returns `{true, _}`.

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

Creates a boolean using `Spec`.

Raises `{missing_metric_spec_key, Key, Spec}` error if required `Soec` key
is missing.<br />
Raises `{invalid_metric_name, Name, Message}` error if metric `Name`
is invalid.<br />
Raises `{invalid_metric_help, Help, Message}` error if metric `Help`
is invalid.<br />
Raises `{invalid_metric_labels, Labels, Message}` error if `Labels`
isn't a list.<br />
Raises `{invalid_label_name, Name, Message}` error if `Name` isn't a valid
label name.<br />
Raises `{mf_already_exists, {Registry, Name}, Message}` error if a boolean
with the same `Spec` already exists.

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

Removes boolean series identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if boolean with name `Name`
can't be found in `Registry`.<br />
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

Resets the value of the boolean identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if boolean with name `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="set-2"></a>

### set/2 ###

`set(Name, Value) -> any()`

Equivalent to [`set(default, Name, [], Value)`](#set-4).

<a name="set-3"></a>

### set/3 ###

`set(Name, LabelValues, Value) -> any()`

Equivalent to [`set(default, Name, LabelValues, Value)`](#set-4).

<a name="set-4"></a>

### set/4 ###

`set(Registry, Name, LabelValues, Value0) -> any()`

Sets the boolean identified by `Registry`, `Name`
and `LabelValues` to `Value`.

Valid "truthy" values:
- `true`;
- `false`;
- `0` -> false;
- `number > 0` -> true;
- `[]` -> false
- `non-empty list` -> true;
- `undefined` -> undefined

Other values will generate `invalid_value` error.

Raises `{invalid_value, Value, Message}` if `Value`
isn't a boolean or `undefined`.<br />
Raises `{unknown_metric, Registry, Name}` error if boolean with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="toggle-1"></a>

### toggle/1 ###

`toggle(Name) -> any()`

Equivalent to [`toggle(default, Name, [])`](#toggle-3).

<a name="toggle-2"></a>

### toggle/2 ###

`toggle(Name, LabelValues) -> any()`

Equivalent to [`toggle(default, Name, LabelValues, Value)`](#toggle-4).

<a name="toggle-3"></a>

### toggle/3 ###

`toggle(Registry, Name, LabelValues) -> any()`

Toggles the boolean identified by `Registry`, `Name`
and `LabelValues`.

If boolean set to undefined, it can't be toggled.

Raises `{invalid_value, undefined, Message}` if boolean is undefined.<br />
Raises `{unknown_metric, Registry, Name}` error if boolean with named `Name`
can't be found in `Registry`.<br />
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

Returns the value of the boolean identified by `Registry`, `Name`
and `LabelValues`. If there is no boolean for `LabelValues`,
returns `undefined`.

Raises `{unknown_metric, Registry, Name}` error if boolean named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="values-2"></a>

### values/2 ###

`values(Registry, Name) -> any()`

