-module(prometheus_gauge).
-export([register/0,
         register/1,
         register/2,
         set/2,
         set/3,
         set/4,
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
  ok = prometheus_registry:register_collector(Registry, prometheus_gauge, Name, Labels, Help).

set({Registry, Name, LabelValues}, Value) ->
    set(Registry, Name, LabelValues, Value);
set(Name, LabelValues) when is_list(LabelValues) ->
    set(default, Name, LabelValues, 1);
set(Name, Value) ->
    set(default, Name, [], Value).

set(Name, LabelValues, Value) ->
    set(default, Name, LabelValues, Value).

set(Registry, Name, LabelValues, Value) ->
    set(?PROMETHEUS_GAUGE_TABLE, Registry, Name, LabelValues, Value).

set(Table, Registry, Name, LabelValues, Value) ->
    case ets:update_element(Table, {Registry, Name, LabelValues}, {1, Value}) of
        false ->
            ok = prometheus_metric:check_mf_exists(Registry, prometheus_gauge, Name, length(LabelValues)),
            case ets:insert_new(Table, {{Registry, Name, LabelValues}, Value}) of
                false -> %% some sneaky process already inserted
                    set(Table, Registry, Name, LabelValues, Value);
                true ->
                    ok
            end;
        true ->
            ok
    end,
    ok.

reset(Name) ->
    reset(default, Name, []).

reset(Name, LabelValues) ->
    reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
    set(Registry, Name, LabelValues, 0).

value(Name) ->
    value(default, Name, []).

value(Name, LabelValues) ->
    value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
    [{_Key, Value}] = ets:lookup(?PROMETHEUS_GAUGE_TABLE, {Registry, Name, LabelValues}),
    Value.

collect_mf(Callback, Registry, Name, Labels, Help) ->
  Callback(gauge, Name, Labels, Help, ets:match(?PROMETHEUS_GAUGE_TABLE, {{Registry, Name, '$1'}, '$3'})).

collect_metrics(_Name, Callback, Values) ->
  [Callback(LabelValues, Value) || [LabelValues, Value] <- Values].
