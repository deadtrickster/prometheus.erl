

# Module prometheus_vm_statistics_collector #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_metrics-2">collect_metrics/2</a></td><td></td></tr><tr><td valign="top"><a href="#collect_mf-2">collect_mf/2</a></td><td></td></tr><tr><td valign="top"><a href="#deregister_cleanup-1">deregister_cleanup/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_metrics-2"></a>

### collect_metrics/2 ###

`collect_metrics(X1, Total) -> any()`

<a name="collect_mf-2"></a>

### collect_mf/2 ###

<pre><code>
collect_mf(Callback, Registry::term()) -&gt; Metrics
</code></pre>

<ul class="definitions"><li><code>Callback = <a href="prometheus_collector.md#type-callback">prometheus_collector:callback()</a></code></li><li><code>Metrics = [<a href="prometheus_model.md#type-Metric">prometheus_model:'Metric'()</a>]</code></li></ul>

<a name="deregister_cleanup-1"></a>

### deregister_cleanup/1 ###

`deregister_cleanup(X1) -> any()`

