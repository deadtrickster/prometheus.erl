

# Module prometheus_metric #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `prometheus_metric` behaviour.__<br /> Required callback functions: `new/1`, `new/2`, `declare/1`, `declare/2`, `reset/1`, `reset/2`, `reset/3`, `value/1`, `value/2`, `value/3`.

<a name="types"></a>

## Data Types ##




### <a name="type-help">help()</a> ###


<pre><code>
help() = binary() | nonempty_string()
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = atom() | binary() | nonempty_string()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = number() | {Count::number(), Sum::number()} | {Buckets::[number(), ...], Sum::number()}
</code></pre>

 histogram

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_mf_exists-4">check_mf_exists/4</a></td><td></td></tr><tr><td valign="top"><a href="#deregister_mf-2">deregister_mf/2</a></td><td></td></tr><tr><td valign="top"><a href="#extract_common_params-1">extract_common_params/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert_mf-5">insert_mf/5</a></td><td></td></tr><tr><td valign="top"><a href="#insert_mf-6">insert_mf/6</a></td><td></td></tr><tr><td valign="top"><a href="#insert_new_mf-5">insert_new_mf/5</a></td><td></td></tr><tr><td valign="top"><a href="#insert_new_mf-6">insert_new_mf/6</a></td><td></td></tr><tr><td valign="top"><a href="#metrics-2">metrics/2</a></td><td></td></tr><tr><td valign="top"><a href="#mf_data-1">mf_data/1</a></td><td></td></tr><tr><td valign="top"><a href="#validate_metric_help-1">validate_metric_help/1</a></td><td></td></tr><tr><td valign="top"><a href="#validate_metric_label_names-1">validate_metric_label_names/1</a></td><td></td></tr><tr><td valign="top"><a href="#validate_metric_name-1">validate_metric_name/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check_mf_exists-4"></a>

### check_mf_exists/4 ###

`check_mf_exists(Table, Registry, Name, LabelValues) -> any()`

<a name="deregister_mf-2"></a>

### deregister_mf/2 ###

`deregister_mf(Table, Registry) -> any()`

<a name="extract_common_params-1"></a>

### extract_common_params/1 ###

`extract_common_params(Spec) -> any()`

<a name="insert_mf-5"></a>

### insert_mf/5 ###

`insert_mf(Table, Registry, Name, Labels, Help) -> any()`

<a name="insert_mf-6"></a>

### insert_mf/6 ###

`insert_mf(Table, Registry, Name, Labels, Help, Data) -> any()`

<a name="insert_new_mf-5"></a>

### insert_new_mf/5 ###

`insert_new_mf(Table, Registry, Name, Labels, Help) -> any()`

<a name="insert_new_mf-6"></a>

### insert_new_mf/6 ###

`insert_new_mf(Table, Registry, Name, Labels, Help, Data) -> any()`

<a name="metrics-2"></a>

### metrics/2 ###

`metrics(Table, Registry) -> any()`

<a name="mf_data-1"></a>

### mf_data/1 ###

`mf_data(MF) -> any()`

<a name="validate_metric_help-1"></a>

### validate_metric_help/1 ###

`validate_metric_help(RawHelp) -> any()`

<a name="validate_metric_label_names-1"></a>

### validate_metric_label_names/1 ###

`validate_metric_label_names(RawLabels) -> any()`

<a name="validate_metric_name-1"></a>

### validate_metric_name/1 ###

`validate_metric_name(RawName) -> any()`

