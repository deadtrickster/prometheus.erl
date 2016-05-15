-module(prometheus_summary).

%%% metric
-export([new/1,
         new/2,
         observe/2,
         observe/3,
         observe/4,
         dobserve/2,
         dobserve/3,
         dobserve/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3]).

%%% collector
-export([register/0,
         register/1,
         collect_mf/2,
         collect_metrics/3]).

%%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         start_link/0]).

-include("prometheus.hrl").
-behaviour(prometheus_collector).
-behaviour(prometheus_metric).
-behaviour(gen_server).

-define(SUM_POS, 3).
-define(COUNTER_POS, 2).

%%====================================================================
%% Metric API
%%====================================================================

new(Spec) ->
  new(Spec, default).

new(Spec, Registry) ->
  {Name, Labels, Help} = prometheus_metric:extract_common_params(Spec),
  %% Value = proplists:get_value(value, Spec),
  register(Registry),
  prometheus_metric:insert_mf(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, Labels, Help).

observe(Name, Value) ->
  observe(default, Name, [], Value).

observe(Name, LabelValues, Value) ->
  observe(default, Name, LabelValues, Value).

observe(Registry, Name, LabelValues, Value) ->
  try
    ets:update_counter(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, [{?COUNTER_POS, 1}, {?SUM_POS, Value}])
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun observe/4)
  end.

dobserve(Name, Value) ->
  dobserve(default, Name, [], Value).

dobserve(Name, LabelValues, Value) ->
  dobserve(default, Name, LabelValues, Value).

dobserve(Registry, Name, LabelValues, Value) ->
  gen_server:cast(prometheus_summary, {observe, {Registry, Name, LabelValues, Value}}).

reset(Name) ->
  reset(default, Name, []).
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, LabelValues),
  ets:update_element(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, [{?COUNTER_POS, 0}, {?SUM_POS, 0}]).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Count, Sum}] = ets:lookup(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}),
  {Count, Sum}.

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

collect_mf(Callback, Registry) ->
  [Callback(summary, Name, Labels, Help, [Registry]) ||
    [Name, Labels, Help, _] <- prometheus_metric:metrics(?PROMETHEUS_SUMMARY_TABLE, Registry)].

collect_metrics(Name, Callback, [Registry]) ->
  [emit_summary_stat(Name, LabelValues, Count, Sum, Callback) ||
    [LabelValues, Count, Sum] <- ets:match(?PROMETHEUS_SUMMARY_TABLE, {{Registry, Name, '$1'}, '$2', '$3'})].

%%====================================================================
%% Gen_server API
%%====================================================================

init(_Args) ->
  {ok, []}.

handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast({observe, {Registry, Name, LabelValues, Value}}, State) ->
  dobserve_impl(Registry, Name, LabelValues, Value),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_link() ->
  gen_server:start_link({local, prometheus_summary}, prometheus_summary, [], []).

%%====================================================================
%% Private Parts
%%====================================================================

dobserve_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}) of
    [Metric] ->
      ets:update_element(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, {?SUM_POS, sum(Metric) + Value}),
      ets:update_counter(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, {?COUNTER_POS, 1});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dobserve_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?PROMETHEUS_SUMMARY_TABLE, {{Registry, Name, LabelValues}, 1, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

sum(Metric) ->
  element(?SUM_POS, Metric).

emit_summary_stat(Name, LabelValues, Count, Sum, Callback) ->
  Callback({atom_to_list(Name) ++ "_count" , LabelValues}, Count),
  Callback({atom_to_list(Name) ++ "_sum" , LabelValues}, Sum).
