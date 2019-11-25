%% @doc
%% Collects statistics about the microstate accounting of Erlang VM threads using
%% <a href="http://erlang.org/doc/man/msacc.html">msacc</a>. Refer to
%% <a href="http://erlang.org/doc/man/erlang.html#statistics_microstate_accounting">
%%   erlang:statistics(microstate_accounting)
%% </a> for more information about thread types and microstates.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%      `erlang_vm_thread_microstates_microseconds{thread_type=Type, thread_state=State, thread_id=ID}'<br/>
%%      Type: counter<br/>
%%      The time spent in the labeled microstate since collection began for the labeled VM thread. <br/>
%%
%%      Labels:
%%      <ul>
%%        <li>`thread_type': `async | aux | dirty_io_scheduler | dirty_cpu_scheduler |
%%             poll | scheduler'</li>
%%        <li>`thread_state': `alloc | aux | bif | busy_wait | check_io | emulator | ets |
%%             gc | gc_fullsweep | nif | other | port | send | sleep | timers'</li>
%%        <li>`thread_id': the numeric ID of the VM thread</li>
%%      </ul>
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% The <a href="http://erlang.org/doc/man/erlang.html#type-time_unit">time
%% unit</a> for this collector can be configured via
%% `vm_microstates_collector_unit' key of `prometheus' app environment. Changing
%% the unit will cause the metric to have a different name and scale the value
%% to the given unit, e.g `second' will result in
%% `erlang_vm_thread_microstates_seconds'.
%%
%% @end
-module(prometheus_vm_microstates_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5, counter_metrics/1]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(METRIC_NAME_PREFIX, "erlang_vm_thread_microstates_").

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) ->
  _ = msacc:stop(),
  ok.

-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
%% @private
collect_mf(_Registry, Callback) ->
  msacc:start(),
  case msacc:available() of
    true ->
      Callback(
        create_counter(metric_name(), "VM thread time spent in each state", msacc:stats())
       ),
      ok;

    _ ->
      ok
  end.


-spec collect_metrics(Name, Data) -> Metrics when
    Name :: iolist(),
    Data :: msacc:msacc_data(),
    Metrics :: [prometheus_model:'Metric'()].
%% @private
collect_metrics(_Name, Data) ->
  Scale = scale(),
  counter_metrics(
    [
     {[{thread_type, ThreadType}, {thread_id, ThreadID}, {thread_state, State}], Time / Scale}
     || #{ type := ThreadType, id := ThreadID, counters := Counters } <- Data,
        {State, Time} <- maps:to_list(Counters)
    ]
   ).

%%====================================================================
%% Private functions
%%====================================================================
create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

metric_name() ->
  ?METRIC_NAME([unit(), $s]).

scale() ->
  erlang:convert_time_unit(1, unit(), native).

unit() ->
  application:get_env(prometheus, vm_microstates_collector_unit, microsecond).
