-module(prometheus_counter).
-export([register/0,
         register/1,
         new/1,
         new/2,
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
         collect_mf/2,
         collect_metrics/3]).

-include("prometheus.hrl").
-behaviour(prometheus_collector).
-behaviour(prometheus_metric).

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

new(Spec) ->
  new(Spec, default).

new(Spec, Registry) ->
  Name = proplists:get_value(name, Spec),
  Labels = proplists:get_value(labels, Spec, []),
  Help = proplists:get_value(help, Spec, ""),
  %% Value = proplists:get_value(value, Spec),
  register(Registry),
  prometheus_metric:insert_mf(?PROMETHEUS_COUNTER_TABLE, Registry, Name, Labels, Help).

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
      prometheus_metric:check_mf_exists(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues),
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
  prometheus_metric:check_mf_exists(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues),
  ets:update_element(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, [{2, 0}]).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Value}] = ets:lookup(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}),
  Value.

collect_mf(Callback, Registry) ->
  [Callback(counter, Name, Labels, Help, [Registry]) || [Name, Labels, Help] <- prometheus_metric:metrics(?PROMETHEUS_COUNTER_TABLE, Registry)].

collect_metrics(Name, Callback, [Registry]) ->
  [Callback(LabelValues, Value) || [LabelValues, Value] <- ets:match(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, '$1'}, '$2'})].
