%% @doc
%% Collects microstate accounting metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#statistics_microstate_accounting">
%%   erlang:statistics(microstate_accounting)
%% </a>.
%%
%% In order for values to increase, microstate
%% accounting must be enabled. This is done by
%% calling <code>erlang:system_flag(microstate_accounting, true).</code>
%%
%% ==Exported metrics==
%% Some metrics are not available by default. They require a VM
%% configured with <code>./configure --with-microstate-accounting=extra</code>.
%%
%% <ul>
%%   <li>
%%     `erlang_vm_msacc_aux'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent handling auxiliary jobs.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_check_io'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent checking for new I/O events.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_emulator'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent executing Erlang processes.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_gc'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent doing garbage collection.
%%     When extra states are enabled this is the time spent
%%     doing non-fullsweep garbage collections.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_other'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent doing unaccounted things.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_port'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent executing ports.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_sleep'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent sleeping.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_alloc'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent managing memory.
%%     Without extra states this time is spread out over all other states.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_bif'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent in BIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_busy_wait'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent busy waiting.
%%     Without extra states this time is part of the 'other' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_ets'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent executing ETS BIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_gc_full'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent doing fullsweep garbage collection.
%%     Without extra states this time is part of the 'gc' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_nif'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent in NIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_send'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent sending messages (processes only).
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_timers'<br/>
%%     Type: counter.<br/>
%%     Time in microseconds spent managing timers.
%%     Without extra states this time is part of the 'other' state.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_msacc_collector_metrics' key of `prometheus' app environment.
%%
%% Options are the same as MSAcc_Thread_State return type from
%% <a href="http://erlang.org/doc/man/erlang.html#statistics_microstate_accounting">
%%   erlang:statistics(microstate_accounting)
%% </a>:
%% <ul>
%%   <li>
%%     `aux' for `erlang_vm_msacc_aux'.
%%   </li>
%%   <li>
%%     `check_io' for `erlang_vm_msacc_check_io'.
%%   </li>
%%   <li>
%%     `emulator' for `erlang_vm_msacc_emulator'.
%%   </li>
%%   <li>
%%     `gc' for `erlang_vm_msacc_gc'.
%%   </li>
%%   <li>
%%     `other' for `erlang_vm_msacc_other'.
%%   </li>
%%   <li>
%%     `port' for `erlang_vm_msacc_port'.
%%   </li>
%%   <li>
%%     `sleep' for `erlang_vm_msacc_sleep'.
%%   </li>
%%   <li>
%%     `alloc' for `erlang_vm_msacc_alloc'.
%%   </li>
%%   <li>
%%     `bif' for `erlang_vm_msacc_bif'.
%%   </li>
%%   <li>
%%     `busy_wait' for `erlang_vm_msacc_busy_wait'.
%%   </li>
%%   <li>
%%     `ets' for `erlang_vm_msacc_ets'.
%%   </li>
%%   <li>
%%     `gc_full' for `erlang_vm_msacc_gc_full'.
%%   </li>
%%   <li>
%%     `nif' for `erlang_vm_msacc_nif'.
%%   </li>
%%   <li>
%%     `send' for `erlang_vm_msacc_send'.
%%   </li>
%%   <li>
%%     `timers' for `erlang_vm_msacc_timers'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled as far as Prometheus is concerned,
%% although some metrics could not be enabled by the VM itself.
%% @end
-module(prometheus_vm_msacc_collector).

-export([deregister_cleanup/1,
         collect_mf/2]).

-import(prometheus_model_helpers, [create_mf/4]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(METRIC_NAME_PREFIX, "erlang_vm_msacc_").

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) -> ok.

-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
%% @private
collect_mf(_Registry, Callback) ->
  Metrics = metrics(),
  EnabledMetrics = enabled_metrics(),
  [add_metric_family(Metric, Callback)
   || {Name, _, _, _}=Metric <- Metrics, metric_enabled(Name, EnabledMetrics)],
  ok.

add_metric_family({Name, Type, Help, Metrics}, Callback) ->
  Callback(create_mf(?METRIC_NAME(Name), Help, Type, Metrics)).

%%====================================================================
%% Private Parts
%%====================================================================

metrics() ->
  Data = erlang:statistics(microstate_accounting),
  [
   %% Base states.
   {aux, counter,
    "Time in microseconds spent handling auxiliary jobs.",
    metric(aux, Data)},
   {check_io, counter,
    "Time in microseconds spent checking for new I/O events.",
    metric(check_io, Data)},
   {emulator, counter,
    "Time in microseconds spent executing Erlang processes.",
    metric(emulator, Data)},
   {gc, counter,
    "Time in microseconds spent doing garbage collection. "
    "When extra states are enabled this is the time spent "
    "doing non-fullsweep garbage collections.",
    metric(gc, Data)},
   {other, counter,
    "Time in microseconds spent doing unaccounted things.",
    metric(other, Data)},
   {port, counter,
    "Time in microseconds spent executing ports.",
    metric(port, Data)},
   {sleep, counter,
    "Time in microseconds spent sleeping.",
    metric(sleep, Data)},
   %% Extra states.
   {alloc, counter,
    "Time in microseconds spent managing memory. "
    "Without extra states this time is spread out over all other states.",
    metric(alloc, Data)},
   {bif, counter,
    "Time in microseconds spent in BIFs. "
    "Without extra states this time is part of the 'emulator' state.",
    metric(bif, Data)},
   {busy_wait, counter,
    "Time in microseconds spent busy waiting. "
    "Without extra states this time is part of the 'other' state.",
    metric(busy_wait, Data)},
   {ets, counter,
    "Time in microseconds spent executing ETS BIFs. "
    "Without extra states this time is part of the 'emulator' state.",
    metric(ets, Data)},
   {gc_full, counter,
    "Time in microseconds spent doing fullsweep garbage collection. "
    "Without extra states this time is part of the 'gc' state.",
    metric(gc_full, Data)},
   {nif, counter,
    "Time in microseconds spent in NIFs. "
    "Without extra states this time is part of the 'emulator' state.",
    metric(nif, Data)},
   {send, counter,
    "Time in microseconds spent sending messages (processes only). "
    "Without extra states this time is part of the 'emulator' state.",
    metric(send, Data)},
   {timers, counter,
    "Time in microseconds spent managing timers. "
    "Without extra states this time is part of the 'other' state.",
    metric(timers, Data)}
  ].

metric(Counter, Data) ->
    [
        {
            [{type, Type}, {id, ID}],
            erlang:convert_time_unit(Value, perf_counter, microsecond)
        }
    || #{type := Type, id := ID, counters := #{Counter := Value}} <- Data].

enabled_metrics() ->
  application:get_env(prometheus, vm_msacc_collector_metrics, all).

metric_enabled(Name, Metrics) ->
  Metrics =:= all orelse lists:member(Name, Metrics).
