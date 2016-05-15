-module(prometheus_vm_statistics_collector).
-export([collect_mf/2,
         collect_metrics/3,
         register/0,
         register/1,
         deregister/1]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

collect_mf(Callback, _Registry) ->
  [call_if_statistics_exists(MFName, fun(Stat) ->
                                         add_metric_family(MFName, Stat, Callback)
                                     end) || MFName <- enabled_statistics_metrics()].

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister(_) -> ok.

add_metric_family(context_switches, Stat, Callback) ->
  Callback(counter, erlang_vm_statistics_context_switches, [], "Total number of context switches since the system started", Stat);
add_metric_family(garbage_collection, Stat, Callback) ->
  Callback(counter, erlang_vm_statistics_garbage_collection_number_of_gcs, [], "Garbage collection: number of GCs", Stat),
  Callback(counter, erlang_vm_statistics_garbage_collection_words_reclaimed, [], "Garbage collection: words reclaimed", Stat);
add_metric_family(io, Stat, Callback) ->
  Callback(counter, erlang_vm_statistics_bytes_received_total, [], "Total number of bytes received through ports", Stat),
  Callback(counter, erlang_vm_statistics_bytes_output_total, [], "Total number of bytes output to ports", Stat);
add_metric_family(reductions, Stat, Callback) ->
  Callback(counter, erlang_vm_statistics_reductions_total, [], "Total reductions", Stat);
add_metric_family(run_queue, Stat, Callback) ->
  Callback(gauge, erlang_vm_statistics_run_queues_length_total, [], "Total length of the run-queues", Stat);
add_metric_family(runtime, Stat, Callback) ->
  Callback(counter, erlang_vm_statistics_runtime_milliseconds, [], "The sum of the runtime for all threads in the Erlang runtime system. Can be greate than wall clock time", Stat);
add_metric_family(wall_clock, Stat, Callback) ->
  Callback(counter, erlang_vm_statistics_wallclock_time_milliseconds, [], "Information about wall clock. Same as erlang_vm_statistics_runtime_milliseconds except that real time is measured", Stat).

collect_metrics(erlang_vm_statistics_context_switches, Callback, {Stat, _}) ->
  Callback([], Stat);
collect_metrics(erlang_vm_statistics_garbage_collection_number_of_gcs, Callback, {NumberOfGCs, _, _}) ->
  Callback([], NumberOfGCs);
collect_metrics(erlang_vm_statistics_garbage_collection_words_reclaimed, Callback, {_, WordsReclaimed, _}) ->
  Callback([], WordsReclaimed);
collect_metrics(erlang_vm_statistics_bytes_received_total, Callback, {{input, Input}, _}) ->
  Callback([], Input);
collect_metrics(erlang_vm_statistics_bytes_output_total, Callback, {_, {output, Output}}) ->
  Callback([], Output);
collect_metrics(erlang_vm_statistics_reductions_total, Callback, {ReductionsTotal, _}) ->
  Callback([], ReductionsTotal);
collect_metrics(erlang_vm_statistics_run_queues_length_total, Callback, Total) ->
  Callback([], Total);
collect_metrics(erlang_vm_statistics_runtime_milliseconds, Callback, {Runtime, _}) ->
  Callback([], Runtime);
collect_metrics(erlang_vm_statistics_wallclock_time_milliseconds, Callback, {WallclockTime, _}) ->
  Callback([], WallclockTime).

call_if_statistics_exists(StatItem, Fun) ->
  try
    Stat = erlang:statistics(StatItem),
    Fun(Stat)
  catch
    error:badarg -> undefined
  end.

enabled_statistics_metrics() ->
  application:get_env(prometheus, vm_statistics_collector_metrics, ?PROMETHEUS_VM_STATISTICS).
