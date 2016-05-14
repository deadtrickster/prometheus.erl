-module(prometheus_summary).
-export([register/0,
         register/1,
         new/1,
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
         value/3,
         collect_mf/2,
         collect_metrics/3]).

-export([start_link/0]).

%%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("prometheus.hrl").
-behaviour(prometheus_collector).
-behaviour(prometheus_metric).
-behaviour(gen_server).

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

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
    ets:update_counter(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, {2, Value}),
    ets:update_counter(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, {3, 1})
  catch error:badarg ->
      prometheus_metric:check_mf_exists(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, LabelValues),
      case ets:insert_new(?PROMETHEUS_SUMMARY_TABLE, {{Registry, Name, LabelValues}, Value, 1}) of
        false -> %% some sneaky process already inserted
          observe(Registry, Name, LabelValues, Value);
        true ->
          ok
      end
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
  ets:update_element(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, [{2, 0}, {3, 0}]).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Sum, Count}] = ets:lookup(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}),
  {Sum, Count}.

collect_mf(Callback, Registry) ->
  [Callback(summary, Name, Labels, Help, [Registry]) || [Name, Labels, Help, _] <- prometheus_metric:metrics(?PROMETHEUS_SUMMARY_TABLE, Registry)].

collect_metrics(Name, Callback, [Registry]) ->
  [emit_summary_stat(Name, LabelValues, Sum, Count, Callback) || [LabelValues, Sum, Count] <- ets:match(?PROMETHEUS_SUMMARY_TABLE, {{Registry, Name, '$1'}, '$2', '$3'})].

emit_summary_stat(Name, LabelValues, Sum, Count, Callback) ->
  Callback({atom_to_list(Name) ++ "_count" , LabelValues}, Count),
  Callback({atom_to_list(Name) ++ "_sum" , LabelValues}, Sum).


start_link() ->
  gen_server:start_link({local, prometheus_summary}, prometheus_summary, [], []).

init(_Args) ->
  {ok, []}.

handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast({observe, {Registry, Name, LabelValues, Value}}, State) ->
  dobserve_impl(Registry, Name, LabelValues, Value),
  {noreply, State}.

dobserve_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}) of
    [] ->
      prometheus_metric:check_mf_exists(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, LabelValues),
      case ets:insert_new(?PROMETHEUS_SUMMARY_TABLE, {{Registry, Name, LabelValues}, Value, 1}) of
        false -> %% some sneaky process already inserted
          dobserve_impl(Registry, Name, LabelValues, Value);
        true ->
          ok
      end;
    [{_Key, Sum, _Counter}] ->
      ets:update_element(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, [{2, Sum + Value}]),
      ets:update_counter(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, {3, 1})
  end.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
