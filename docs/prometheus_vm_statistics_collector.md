

# Module prometheus_vm_statistics_collector #
* [Description](#description)

Collects Erlang VM metrics using
[
erlang:statistics/1
](http://erlang.org/doc/man/erlang.md#statistics-1).

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##


### <a name="Exported_metrics">Exported metrics</a> ###


* 
```
erlang_vm_statistics_bytes_output_total
```

The total number of bytes output to ports.

* 
```
erlang_vm_statistics_bytes_received_total
```

The total number of bytes received through ports.

* 
```
erlang_vm_statistics_context_switches
```

The total number of context switches since the system started.

* 
```
erlang_vm_statistics_garbage_collection_number_of_gcs
```

The total number of garbage collections since the system started.

* 
```
erlang_vm_statistics_garbage_collection_words_reclaimed
```

The total number of words reclaimed by GC since the system started.

* 
```
erlang_vm_statistics_reductions_total
```

Total reductions count.

* 
```
erlang_vm_statistics_run_queues_length_total
```

The total length of the run-queues. That is, the number of
processes and ports that are ready to run on all available run-queues.

* 
```
erlang_vm_statistics_runtime_milliseconds
```

The sum of the runtime for all threads in the Erlang runtime system.

* 
```
erlang_vm_statistics_wallclock_time_milliseconds
```

Can be used in the same manner as
`erlang_vm_statistics_runtime_milliseconds`, except that real time is
measured as opposed to runtime or CPU time.



### <a name="Configuration">Configuration</a> ###

Metrics exported by this collector can be configured via
`vm_statistics_collector_metrics` of `prometheus` app environment key.

Options are the same as Item parameter values for
[
erlang:statistics/1
](http://erlang.org/doc/man/erlang.md#statistics-1):

* `context_switches` for `erlang_vm_statistics_context_switches`.

* `garbage_collection`
for `erlang_vm_statistics_garbage_collection_number_of_gcs` and
`erlang_vm_statistics_garbage_collection_words_reclaimed`.

* `io` for `erlang_vm_statistics_bytes_output_total` and
`erlang_vm_statistics_bytes_received_total`.

* `reductions` for `erlang_vm_statistics_reductions_total`.

* `run_queue` for `erlang_vm_statistics_run_queues_length_total`.

* `runtime` for `erlang_vm_statistics_runtime_milliseconds`.

* `wall_clock` for `erlang_vm_statistics_wallclock_time_milliseconds`.


By default all metrics are enabled.