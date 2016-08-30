-module(prometheus_vm_statistics_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(GC_NUM_GCS,
        erlang_vm_statistics_garbage_collection_number_of_gcs).
-define(GC_WORDS_RECLAIMED,
        erlang_vm_statistics_garbage_collection_words_reclaimed).
-define(BYTES_RECEIVED, erlang_vm_statistics_bytes_received_total).
-define(BYTES_OUTPUT, erlang_vm_statistics_bytes_output_total).
-define(REDUCTIONS, erlang_vm_statistics_reductions_total).
-define(RUN_QUEUES_LENGTH, erlang_vm_statistics_run_queues_length_total).
-define(RUNTIME_MS, erlang_vm_statistics_runtime_milliseconds).
-define(CONTEXT_SWITCHES, erlang_vm_statistics_context_switches).
-define(WALLCLOCK_TIME_MS, erlang_vm_statistics_wallclock_time_milliseconds).

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) -> ok.

-spec collect_mf(Callback, _) -> Metrics when
    Callback :: prometheus_collector:callback(),
    Metrics  :: [prometheus_model:'Metric'()].
%% @private
collect_mf(Callback, _Registry) ->
  [call_if_statistics_exists(MFName,
                             fun(Stat) ->
                                 add_metric_family(MFName, Stat, Callback)
                             end)
   || MFName <- enabled_statistics_metrics()].

add_metric_family(context_switches, Stat, Callback) ->
  do_add_metric_family(?CONTEXT_SWITCHES, Stat, Callback,
                       "Total number of context switches "
                       "since the system started");
add_metric_family(garbage_collection, Stat, Callback) ->
  do_add_metric_family(?GC_NUM_GCS, Stat, Callback,
                       "Garbage collection: number of GCs"),
  do_add_metric_family(?GC_WORDS_RECLAIMED, Stat, Callback,
                       "Garbage collection: words reclaimed");
add_metric_family(io, Stat, Callback) ->
  do_add_metric_family(?BYTES_RECEIVED, Stat, Callback,
                       "Total number of bytes received through ports"),
  do_add_metric_family(?BYTES_OUTPUT, Stat, Callback,
                       "Total number of bytes output to ports");
add_metric_family(reductions, Stat, Callback) ->
  do_add_metric_family(?REDUCTIONS, Stat, Callback, "Total reductions");
add_metric_family(run_queue, Stat, Callback) ->
  do_add_metric_family(?RUN_QUEUES_LENGTH, Stat, Callback,
                       "Total length of the run-queues");
add_metric_family(runtime, Stat, Callback) ->
  do_add_metric_family(?RUNTIME_MS, Stat, Callback,
                       "The sum of the runtime for all threads "
                       "in the Erlang runtime system. "
                       "Can be greater than wall clock time");
add_metric_family(wall_clock, Stat, Callback) ->
  do_add_metric_family(
    erlang_vm_statistics_wallclock_time_milliseconds,
    Stat, Callback,
    "Information about wall clock. "
    "Same as erlang_vm_statistics_runtime_milliseconds "
    "except that real time is measured").

do_add_metric_family(Name, Stat, Callback, Help) ->
  Callback(create_counter(Name, Help, Stat)).

%% @private
collect_metrics(erlang_vm_statistics_context_switches, {Stat, _}) ->
  counter_metric(Stat);
collect_metrics(?GC_NUM_GCS, {NumberOfGCs, _, _}) ->
  counter_metric(NumberOfGCs);
collect_metrics(?GC_WORDS_RECLAIMED, {_, WordsReclaimed, _}) ->
  counter_metric(WordsReclaimed);
collect_metrics(?BYTES_RECEIVED, {{input, Input}, _}) ->
  counter_metric(Input);
collect_metrics(?BYTES_OUTPUT, {_, {output, Output}}) ->
  counter_metric(Output);
collect_metrics(?REDUCTIONS, {ReductionsTotal, _}) ->
  counter_metric(ReductionsTotal);
collect_metrics(?RUN_QUEUES_LENGTH, Total) ->
  gauge_metric(Total);
collect_metrics(?RUNTIME_MS, {Runtime, _}) ->
  counter_metric(Runtime);
collect_metrics(?WALLCLOCK_TIME_MS, {WallclockTime, _}) ->
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
  application:get_env(prometheus, vm_statistics_collector_metrics,
                      ?PROMETHEUS_VM_STATISTICS).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

%% create_gauge(Name, Help, Data) ->
%%   create_mf(Name, Help, gauge, ?MODULE, Data).
