

# Module prometheus_vm_microstates_collector #
* [Description](#description)

Collects statistics about the microstate accounting of Erlang VM threads using
[msacc](http://erlang.org/doc/man/msacc.html).

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##

Refer to
[
erlang:statistics(microstate_accounting)
](http://erlang.org/doc/man/erlang.html#statistics_microstate_accounting) for more information about thread types and microstates.


### <a name="Exported_metrics">Exported metrics</a> ###


* `erlang_vm_thread_microstates_microseconds{thread_type="async|aux|dirty_io_scheduler|dirty_cpu_scheduler|poll|scheduler", thread_state="alloc|aux|bif|busy_wait|check_io|emulator|ets|gc|gc_fullsweep|nif|other|port|send|sleep|timers", thread_id=ID}`<br />
Type: counter<br />
The time spent in the labeled microstate since collection began for the labeled VM thread.



### <a name="Configuration">Configuration</a> ###

The [time
unit](http://erlang.org/doc/man/erlang.html#type-time_unit) for this collector can be configured via
`vm_microstates_collector_unit` key of `prometheus` app environment. Changing
the unit will cause the metric to have a different name and scale the value
to the given unit, e.g `second` will result in
`erlang_vm_thread_microstates_seconds`.
