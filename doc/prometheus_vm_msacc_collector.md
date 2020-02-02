

# Module prometheus_vm_msacc_collector #
* [Description](#description)

Collects microstate accounting metrics using
[
erlang:statistics(microstate_accounting)
](http://erlang.org/doc/man/erlang.md#statistics_microstate_accounting).

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##

In order for values to increase, microstate
accounting must be enabled. This is done by
calling `erlang:system_flag(microstate_accounting, true).`


### <a name="Exported_metrics">Exported metrics</a> ###

Some metrics are not available by default. They require a VM
configured with `./configure --with-microstate-accounting=extra`.

* `erlang_vm_msacc_aux_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent handling auxiliary jobs.

* `erlang_vm_msacc_check_io_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent checking for new I/O events.

* `erlang_vm_msacc_emulator_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent executing Erlang processes.

* `erlang_vm_msacc_gc_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent doing garbage collection.
When extra states are enabled this is the time spent
doing non-fullsweep garbage collections.

* `erlang_vm_msacc_other_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent doing unaccounted things.

* `erlang_vm_msacc_port_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent executing ports.

* `erlang_vm_msacc_sleep_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent sleeping.

* `erlang_vm_msacc_alloc_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent managing memory.
Without extra states this time is spread out over all other states.

* `erlang_vm_msacc_bif_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent in BIFs.
Without extra states this time is part of the 'emulator' state.

* `erlang_vm_msacc_busy_wait_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent busy waiting.
Without extra states this time is part of the 'other' state.

* `erlang_vm_msacc_ets_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent executing ETS BIFs.
Without extra states this time is part of the 'emulator' state.

* `erlang_vm_msacc_gc_full_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent doing fullsweep garbage collection.
Without extra states this time is part of the 'gc' state.

* `erlang_vm_msacc_nif_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent in NIFs.
Without extra states this time is part of the 'emulator' state.

* `erlang_vm_msacc_send_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent sending messages (processes only).
Without extra states this time is part of the 'emulator' state.

* `erlang_vm_msacc_timers_seconds_total`<br />
Type: counter.<br />
Total time in seconds spent managing timers.
Without extra states this time is part of the 'other' state.



### <a name="Configuration">Configuration</a> ###

Metrics exported by this collector can be configured via
`vm_msacc_collector_metrics` key of `prometheus` app environment.

Options are the same as MSAcc_Thread_State return type from
[
erlang:statistics(microstate_accounting)
](http://erlang.org/doc/man/erlang.md#statistics_microstate_accounting) with `_seconds_total` as the suffix:

* `aux_seconds_total` for `erlang_vm_msacc_aux_seconds_total`.

* `check_io_seconds_total` for `erlang_vm_msacc_check_io_seconds_total`.

* `emulator_seconds_total` for `erlang_vm_msacc_emulator_seconds_total`.

* `gc_seconds_total` for `erlang_vm_msacc_gc_seconds_total`.

* `other_seconds_total` for `erlang_vm_msacc_other_seconds_total`.

* `port_seconds_total` for `erlang_vm_msacc_port_seconds_total`.

* `sleep_seconds_total` for `erlang_vm_msacc_sleep_seconds_total`.

* `alloc_seconds_total` for `erlang_vm_msacc_alloc_seconds_total`.

* `bif_seconds_total` for `erlang_vm_msacc_bif_seconds_total`.

* `busy_wait_seconds_total` for `erlang_vm_msacc_busy_wait_seconds_total`.

* `ets_seconds_total` for `erlang_vm_msacc_ets_seconds_total`.

* `gc_full_seconds_total` for `erlang_vm_msacc_gc_full_seconds_total`.

* `nif_seconds_total` for `erlang_vm_msacc_nif_seconds_total`.

* `send_seconds_total` for `erlang_vm_msacc_send_seconds_total`.

* `timers_seconds_total` for `erlang_vm_msacc_timers_seconds_total`.


By default all metrics are enabled as far as Prometheus is concerned,
although some metrics could not be enabled by the VM itself.