-module(prometheus_gauge).

%%% metric
-export([new/1,
         new/2,
         set/2,
         set/3,
         set/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3]).

%%% collector
-export([register/0,
         register/1,
         deregister/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").
-behaviour(prometheus_collector).
-behaviour(prometheus_metric).

-define(TABLE, ?PROMETHEUS_GAUGE_TABLE).
-define(GAUGE_POS, 2).

%%====================================================================
%% Metric API
%%====================================================================

new(Spec) ->
  new(Spec, default).

new(Spec, Registry) ->
  {Name, Labels, Help} = prometheus_metric:extract_common_params(Spec),
  %% Value = proplists:get_value(value, Spec),
  register(Registry),
  prometheus_metric:insert_mf(?TABLE, Registry, Name, Labels, Help).

set(Name, Value) ->
  set(default, Name, [], Value).

set(Name, LabelValues, Value) ->
  set(default, Name, LabelValues, Value).

set(Registry, Name, LabelValues, Value) when is_number(Value) ->
  case ets:update_element(?TABLE, {Registry, Name, LabelValues}, {?GAUGE_POS, Value}) of
    false ->
      insert_metric(Registry, Name, LabelValues, Value, fun set/4);
    true ->
      ok
  end,
  ok;
set(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "set accepts only numbers"}).


reset(Name) ->
  reset(default, Name, []).

reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  ets:update_element(?TABLE, {Registry, Name, LabelValues}, {?GAUGE_POS, 0}).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Value}] = ets:lookup(?TABLE, {Registry, Name, LabelValues}),
  Value.

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  ets:match_delete(?TABLE, {{Registry, '_', '_'}, '_'}).

collect_mf(Callback, Registry) ->
  [Callback(create_mf(Name, gauge, Help, {Labels, Registry})) ||
    [Name, Labels, Help, _] <- prometheus_metric:metrics(?TABLE, Registry)].

collect_metrics(Name, {Labels, Registry}) ->
  [gauge_metric(lists:zip(Labels, LabelValues), Value) ||
    [LabelValues, Value] <- ets:match(?TABLE, {{Registry, Name, '$1'}, '$2'})].

%%====================================================================
%% Private Parts
%%====================================================================

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {{Registry, Name, LabelValues}, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

create_mf(Name, Help, Type, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, Type, ?MODULE, Data).
