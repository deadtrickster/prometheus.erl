-module(prometheus_vm_statistics_collector).
-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(_) -> ok.

collect_mf(Callback, _Registry) ->
  [call_if_statistics_exists(MFName, fun(Stat) ->
                                         add_metric_family(MFName, Stat, Callback)
                                     end) || MFName <- enabled_statistics_metrics()].

add_metric_family(context_switches, Stat, Callback) ->
  Callback(create_counter(erlang_vm_statistics_context_switches,
                          "Total number of context switches since the system started", Stat));
add_metric_family(garbage_collection, Stat, Callback) ->
  Callback(create_counter(erlang_vm_statistics_garbage_collection_number_of_gcs,
                          "Garbage collection: number of GCs",
                          Stat)),
  Callback(create_counter(erlang_vm_statistics_garbage_collection_words_reclaimed,
                          "Garbage collection: words reclaimed",
                          Stat));
add_metric_family(io, Stat, Callback) ->
  Callback(create_counter(erlang_vm_statistics_bytes_received_total,
                          "Total number of bytes received through ports",
                          Stat)),
  Callback(create_counter(erlang_vm_statistics_bytes_output_total,
                          "Total number of bytes output to ports",
                          Stat));
add_metric_family(reductions, Stat, Callback) ->
  Callback(create_counter(erlang_vm_statistics_reductions_total,
                          "Total reductions",
                          Stat));
add_metric_family(run_queue, Stat, Callback) ->
  Callback(create_gauge(erlang_vm_statistics_run_queues_length_total,
                        "Total length of the run-queues",
                        Stat));
add_metric_family(runtime, Stat, Callback) ->
  Callback(create_counter(erlang_vm_statistics_runtime_milliseconds,
                          "The sum of the runtime for all threads in the Erlang runtime system. Can be greate than wall clock time",
                          Stat));
add_metric_family(wall_clock, Stat, Callback) ->
  Callback(create_counter(erlang_vm_statistics_wallclock_time_milliseconds,
                          "Information about wall clock. Same as erlang_vm_statistics_runtime_milliseconds except that real time is measured",
                          Stat)).

collect_metrics(erlang_vm_statistics_context_switches, {Stat, _}) ->
  counter_metric(Stat);
collect_metrics(erlang_vm_statistics_garbage_collection_number_of_gcs, {NumberOfGCs, _, _}) ->
  counter_metric(NumberOfGCs);
collect_metrics(erlang_vm_statistics_garbage_collection_words_reclaimed, {_, WordsReclaimed, _}) ->
  counter_metric(WordsReclaimed);
collect_metrics(erlang_vm_statistics_bytes_received_total, {{input, Input}, _}) ->
  counter_metric(Input);
collect_metrics(erlang_vm_statistics_bytes_output_total, {_, {output, Output}}) ->
  counter_metric(Output);
collect_metrics(erlang_vm_statistics_reductions_total, {ReductionsTotal, _}) ->
  counter_metric(ReductionsTotal);
collect_metrics(erlang_vm_statistics_run_queues_length_total, Total) ->
  gauge_metric(Total);
collect_metrics(erlang_vm_statistics_runtime_milliseconds, {Runtime, _}) ->
  counter_metric(Runtime);
collect_metrics(erlang_vm_statistics_wallclock_time_milliseconds, {WallclockTime, _}) ->
  counter_metric(WallclockTime).

%%====================================================================
%% Private Parts
%%====================================================================

call_if_statistics_exists(StatItem, Fun) ->
  try
    Stat = erlang:statistics(StatItem),
    Fun(Stat)
  catch
    error:badarg -> undefined
  end.

enabled_statistics_metrics() ->
  application:get_env(prometheus, vm_statistics_collector_metrics, ?PROMETHEUS_VM_STATISTICS).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

%% create_gauge(Name, Help, Data) ->
%%   create_mf(Name, Help, gauge, ?MODULE, Data).
