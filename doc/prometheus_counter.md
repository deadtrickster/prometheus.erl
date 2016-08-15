

# Module prometheus_counter #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md), [`prometheus_collector`](prometheus_collector.md), [`prometheus_metric`](prometheus_metric.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#collect_metrics-2">collect_metrics/2</a></td><td></td></tr><tr><td valign="top"><a href="#collect_mf-2">collect_mf/2</a></td><td></td></tr><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td></td></tr><tr><td valign="top"><a href="#declare-2">declare/2</a></td><td></td></tr><tr><td valign="top"><a href="#deregister_cleanup-1">deregister_cleanup/1</a></td><td></td></tr><tr><td valign="top"><a href="#dinc-1">dinc/1</a></td><td></td></tr><tr><td valign="top"><a href="#dinc-2">dinc/2</a></td><td></td></tr><tr><td valign="top"><a href="#dinc-3">dinc/3</a></td><td></td></tr><tr><td valign="top"><a href="#dinc-4">dinc/4</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#inc-1">inc/1</a></td><td></td></tr><tr><td valign="top"><a href="#inc-2">inc/2</a></td><td></td></tr><tr><td valign="top"><a href="#inc-3">inc/3</a></td><td></td></tr><tr><td valign="top"><a href="#inc-4">inc/4</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td></td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td></td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td></td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="collect_metrics-2"></a>

### collect_metrics/2 ###

`collect_metrics(Name, X2) -> any()`

<a name="collect_mf-2"></a>

### collect_mf/2 ###

`collect_mf(Callback, Registry) -> any()`

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

<a name="declare-2"></a>

### declare/2 ###

`declare(Spec, Registry) -> any()`

<a name="deregister_cleanup-1"></a>

### deregister_cleanup/1 ###

`deregister_cleanup(Registry) -> any()`

<a name="dinc-1"></a>

### dinc/1 ###

`dinc(Name) -> any()`

<a name="dinc-2"></a>

### dinc/2 ###

`dinc(Name, LabelValues) -> any()`

<a name="dinc-3"></a>

### dinc/3 ###

`dinc(Name, LabelValues, Value) -> any()`

<a name="dinc-4"></a>

### dinc/4 ###

`dinc(Registry, Name, LabelValues, Value) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Call, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(X1, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="inc-1"></a>

### inc/1 ###

`inc(Name) -> any()`

<a name="inc-2"></a>

### inc/2 ###

`inc(Name, LabelValues) -> any()`

<a name="inc-3"></a>

### inc/3 ###

`inc(Name, LabelValues, Value) -> any()`

<a name="inc-4"></a>

### inc/4 ###

`inc(Registry, Name, LabelValues, Value) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(Args) -> any()`

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

<a name="new-2"></a>

### new/2 ###

`new(Spec, Registry) -> any()`

<a name="reset-1"></a>

### reset/1 ###

`reset(Name) -> any()`

<a name="reset-2"></a>

### reset/2 ###

`reset(Name, LabelValues) -> any()`

<a name="reset-3"></a>

### reset/3 ###

`reset(Registry, Name, LabelValues) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="value-1"></a>

### value/1 ###

`value(Name) -> any()`

<a name="value-2"></a>

### value/2 ###

`value(Name, LabelValues) -> any()`

<a name="value-3"></a>

### value/3 ###

`value(Registry, Name, LabelValues) -> any()`

