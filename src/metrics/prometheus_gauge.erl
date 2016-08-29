%% @doc
%% Gauge metric, to report instantaneous values.
%%
%% Gauge is a metric that represents a single numerical value that can
%% arbitrarily go up and down.
%%
%% A Gauge is typically used for measured values like temperatures or current
%% memory usage, but also "counts" that can go up and down, like the number of
%% running processes.
%%
%% Example use cases for Gauges:
%% <ul>
%%   <li>Inprogress requests</li>
%%   <li>Number of items in a queue</li>
%%   <li>Free memory</li>
%%   <li>Total memory</li>
%%   <li>Temperature</li>
%% </ul>
%% @end
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

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_GAUGE_TABLE).
-define(GAUGE_POS, 2).

%%====================================================================
%% Metric API
%%====================================================================

new(Spec) ->
  {Registry, Name, Labels, Help} =
    prometheus_metric:extract_common_params(Spec),
  prometheus_collector:register(?MODULE, Registry),
  prometheus_metric:insert_new_mf(?TABLE, Registry, Name, Labels, Help).

%% @deprecated Please use {@link new/1} with registry
%% key instead.
new(Spec, Registry) ->
  ?DEPRECATED("prometheus_gauge:new/2", "prometheus_gauge:new/1"
              " with registry key"),
  new([{registry, Registry} | Spec]).

declare(Spec) ->
  {Registry, Name, Labels, Help} =
    prometheus_metric:extract_common_params(Spec),
  prometheus_collector:register(?MODULE, Registry),
  prometheus_metric:insert_mf(?TABLE, Registry, Name, Labels, Help).

%% @deprecated Please use {@link declare/1} with registry
%% key instead.
declare(Spec, Registry) ->
  ?DEPRECATED("prometheus_gauge:declare/2", "prometheus_gauge:declare/1"
              " with registry key"),
  declare([{registry, Registry} | Spec]).

%% @equiv set(default, Name, [], Value)
set(Name, Value) ->
  set(default, Name, [], Value).

%% @equiv set(default, Name, LabelValues, Value)
set(Name, LabelValues, Value) ->
  set(default, Name, LabelValues, Value).

set(Registry, Name, LabelValues, Value) when is_number(Value) ->
  case ets:update_element(?TABLE, {Registry, Name, LabelValues},
                          {?GAUGE_POS, Value}) of
    false ->
      insert_metric(Registry, Name, LabelValues, Value, fun set/4);
    true ->
      ok
  end,
  ok;
set(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "set accepts only numbers"}).

%% @equiv set_to_current_time(default, Name, [])
set_to_current_time(Name) ->
  set_to_current_time(default, Name, []).

%% @equiv set_to_current_time(default, Name, LabelValues)
set_to_current_time(Name, LabelValues) ->
  set_to_current_time(default, Name, LabelValues).

set_to_current_time(Registry, Name, LabelValues) ->
  set(Registry, Name, LabelValues, os:system_time(seconds)).

%% @equiv track_inprogress(default, Name, [], Fun)
track_inprogress(Name, Fun) ->
  track_inprogress(default, Name, [], Fun).

%% @equiv track_inprogress(default, Name, LabelValues, Fun)
track_inprogress(Name, LabelValues, Fun) ->
  track_inprogress(default, Name, LabelValues, Fun).

track_inprogress(Registry, Name, LabelValues, Fun) ->
  inc(Registry, Name, LabelValues),
  try
    Fun()
  after
    dec(Registry, Name, LabelValues)
  end.

%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  ets:update_element(?TABLE, {Registry, Name, LabelValues}, {?GAUGE_POS, 0}).

%% @equiv value(default, Name, [])
value(Name) ->
  value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, Value}] -> Value;
    [] -> undefined
  end.

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  true = ets:match_delete(?TABLE, {{Registry, '_', '_'}, '_'}),
  ok.

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
  try
    ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                       {?GAUGE_POS, Inc})
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
