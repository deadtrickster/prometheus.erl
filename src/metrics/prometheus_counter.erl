-module(prometheus_counter).
-export([register/0,
         register/1,
         new/1,
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
  Name = proplists:get_value(name, Spec),
  Labels = proplists:get_value(labels, Spec, []),
  Help = proplists:get_value(help, Spec, ""),
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
  try ets:update_counter(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, Value)
  catch error:badarg ->
      prometheus_metric:check_mf_exists(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues),
      case ets:insert_new(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, LabelValues}, Value}) of
        false -> %% some sneaky process already inserted
          inc(Registry, Name, LabelValues, Value);
        true ->
          ok
      end
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
  ets:update_element(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, [{2, 0}]).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Value}] = ets:lookup(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}),
  Value.

collect_mf(Callback, Registry) ->
  [Callback(counter, Name, Labels, Help, [Registry]) || [Name, Labels, Help, _] <- prometheus_metric:metrics(?PROMETHEUS_COUNTER_TABLE, Registry)].

collect_metrics(Name, Callback, [Registry]) ->
  [Callback(LabelValues, Value) || [LabelValues, Value] <- ets:match(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, '$1'}, '$2'})].



start_link() ->
  gen_server:start_link({local, prometheus_counter}, prometheus_counter, [], []).

init(_Args) ->
  {ok, []}.

handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast({inc, {Registry, Name, LabelValues, Value}}, State) ->
  dinc_impl(Registry, Name, LabelValues, Value),
  {noreply, State}.

dinc_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}) of
    [] ->
      prometheus_metric:check_mf_exists(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues),
      case ets:insert_new(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, LabelValues}, Value}) of
        false -> %% some sneaky process already inserted
          dinc_impl(Registry, Name, LabelValues, Value);
        true ->
          ok
      end;
    [{_key, OldValue}] ->
      ets:update_element(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, [{2, Value + OldValue}])
  end.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
