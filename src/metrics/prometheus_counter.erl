-module(prometheus_counter).
-export([register/0,
         register/1,
         register/2,
         inc/1,
         inc/2,
         inc/3,
         inc/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3,
         collect_mf/5,
         collect_metrics/3]).

-include("prometheus.hrl").
-compile({no_auto_import,[register/2]}).
-behaviour(prometheus_collector).

register() ->
  erlang:error(invalid_register_call).

register(Spec) ->
  register(Spec, default).

register(Spec, Registry) ->
  Name = proplists:get_value(name, Spec),
  Labels = proplists:get_value(labels, Spec, []),
  Help = proplists:get_value(help, Spec, ""),
  %Value = proplists:get_value(value, Spec),
  ok = prometheus_registry:register_collector(Registry, prometheus_counter, Name, Labels, Help).

inc(Name) ->
  inc(default, Name, [], 1).

inc(Name, LabelValues) ->
  inc(default, Name, LabelValues, 1).

inc(Name, LabelValues, Value) ->
  inc(default, Name, LabelValues, Value).

inc(Registry, Name, LabelValues, Value) ->
  inc(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues, Value).

inc(Table, Registry, Name, LabelValues, Value) ->
  try ets:update_counter(Table, {Registry, Name, LabelValues}, Value)
  catch error:badarg ->
      ok = prometheus_metric:check_mf_exists(Registry, prometheus_counter, Name, length(LabelValues)),
      case ets:insert_new(Table, {{Registry, Name, LabelValues}, Value}) of
        false -> %% some sneaky process already inserted
          inc(Table, Registry, Name, LabelValues, Value);
        true ->
          ok
      end
  end,
  ok.

reset(Name) ->
  reset(default, Name, []).

reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  ok = prometheus_metric:check_mf_exists(Registry, counter, Name, length(LabelValues)),
  ets:update_counter(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, [{2, 1, 0, 0}]).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Value}] = ets:lookup(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}),
  Value.

collect_mf(Callback, Registry, Name, Labels, Help) ->
  Callback(counter, Name, Labels, Help, ets:match(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, '$1'}, '$3'})).

collect_metrics(_Name, Callback, Values) ->
  [Callback(LabelValues, Value) || [LabelValues, Value] <- Values].
  
