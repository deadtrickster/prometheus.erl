-module(prometheus_counter).

%%% metric
-export([new/1,
         new/2,
         inc/1,
         inc/2,
         inc/3,
         inc/4,
         dinc/1,
         dinc/2,
         dinc/3,
         dinc/4,
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

-define(SUM_POS, 2).

%%====================================================================
%% Metric API
%%====================================================================

new(Spec) ->
  new(Spec, default).

new(Spec, Registry) ->
  {Name, Labels, Help} = prometheus_metric:extract_common_params(Spec),
  %% Value = proplists:get_value(value, Spec),
  register(Registry),
  prometheus_metric:insert_mf(?PROMETHEUS_COUNTER_TABLE, Registry, Name, Labels, Help).

inc(Name) ->
  inc(default, Name, [], 1).

inc(Name, LabelValues) when is_list(LabelValues)->
  inc(default, Name, LabelValues, 1);
inc(Name, Value) when is_number(Value) ->
  inc(default, Name, [], Value).

inc(Name, LabelValues, Value) ->
  inc(default, Name, LabelValues, Value).

inc(Registry, Name, LabelValues, Value) ->
  try ets:update_counter(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, {?SUM_POS, Value})
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun inc/4)
  end,
  ok.

dinc(Name) ->
  dinc(default, Name, [], 1).

dinc(Name, LabelValues) when is_list(LabelValues)->
  dinc(default, Name, LabelValues, 1);
dinc(Name, Value) when is_number(Value) ->
  dinc(default, Name, [], Value).

dinc(Name, LabelValues, Value) ->
  dinc(default, Name, LabelValues, Value).

dinc(Registry, Name, LabelValues, Value) ->
  gen_server:cast(prometheus_counter, {inc, {Registry, Name, LabelValues, Value}}),
  ok.

reset(Name) ->
  reset(default, Name, []).

reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues),
  ets:update_element(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, {?SUM_POS, 0}).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Value}] = ets:lookup(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}),
  Value.

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

collect_mf(Callback, Registry) ->
  [Callback(counter, Name, Labels, Help, [Registry]) ||
    [Name, Labels, Help, _] <- prometheus_metric:metrics(?PROMETHEUS_COUNTER_TABLE, Registry)].

collect_metrics(Name, Callback, [Registry]) ->
  [Callback(LabelValues, Value) ||
    [LabelValues, Value] <- ets:match(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, '$1'}, '$2'})].

%%====================================================================
%% Gen_server API
%%====================================================================

init(_Args) ->
  {ok, []}.

handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast({inc, {Registry, Name, LabelValues, Value}}, State) ->
  dinc_impl(Registry, Name, LabelValues, Value),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_link() ->
  gen_server:start_link({local, prometheus_counter}, prometheus_counter, [], []).

%%====================================================================
%% Private Parts
%%====================================================================

dinc_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}) of
    [{_key, OldValue}] ->
      ets:update_element(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, {?SUM_POS, Value + OldValue});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dinc_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, LabelValues}, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.
