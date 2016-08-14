-module(prometheus_gauge).

%%% metric
-export([new/1,
         new/2,
         declare/1,
         declare/2,
         set/2,
         set/3,
         set/4,
         set_to_current_time/1,
         set_to_current_time/2,
         set_to_current_time/3,
         track_inprogress/2,
         track_inprogress/3,
         track_inprogress/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3]).

%%% collector
-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
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
  prometheus_collector:register(?MODULE, Registry),
  prometheus_metric:insert_new_mf(?TABLE, Registry, Name, Labels, Help).

declare(Spec) ->
  declare(Spec, default).

declare(Spec, Registry) ->
  {Name, Labels, Help} = prometheus_metric:extract_common_params(Spec),
  prometheus_collector:register(?MODULE, Registry),
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

set_to_current_time(Name) ->
  set_to_current_time(default, Name, []).

set_to_current_time(Name, LabelValues) ->
  set_to_current_time(default, Name, LabelValues).

set_to_current_time(Registry, Name, LabelValues) ->
  set(Registry, Name, LabelValues, os:system_time(seconds)).

track_inprogress(Name, Fun) ->
  track_inprogress(default, Name, [], Fun).

track_inprogress(Name, LabelValues, Fun) ->
  track_inprogress(default, Name, LabelValues, Fun).

track_inprogress(Registry, Name, LabelValues, Fun) ->
  inc(Registry, Name, LabelValues),
  try
    Fun()
  after
    dec(Registry, Name, LabelValues)
  end.

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

deregister_cleanup(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  ets:match_delete(?TABLE, {{Registry, '_', '_'}, '_'}).

collect_mf(Callback, Registry) ->
  [Callback(create_gauge(Name, Help, {Labels, Registry})) ||
    [Name, Labels, Help, _] <- prometheus_metric:metrics(?TABLE, Registry)].

collect_metrics(Name, {Labels, Registry}) ->
  [gauge_metric(lists:zip(Labels, LabelValues), Value) ||
    [LabelValues, Value] <- ets:match(?TABLE, {{Registry, Name, '$1'}, '$2'})].

%%====================================================================
%% Private Parts
%%====================================================================

inc(Registry, Name, LabelValues) ->
  inc(Registry, Name, LabelValues, 1).

inc(Registry, Name, LabelValues, Inc) ->
  try ets:update_counter(?TABLE, {Registry, Name, LabelValues}, {?GAUGE_POS, Inc})
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Inc, fun inc/4)
  end,
  ok.

dec(Registry, Name, LabelValues) ->
  inc(Registry, Name, LabelValues, -1).

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {{Registry, Name, LabelValues}, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

create_gauge(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).
