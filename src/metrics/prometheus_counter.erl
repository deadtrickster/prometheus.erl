%% @doc
%% Counter is a Metric that represents a single numerical value that only ever
%% goes up. That implies that it cannot be used to count items whose number can
%% also go down, e.g. the number of currently running processes. Those
%% "counters" are represented by {@link prometheus_gauge}.
%%
%% A Counter is typically used to count requests served, tasks completed, errors
%% occurred, etc.
%%
%% Examople use cases for Counters:
%% <ul>
%%   <li>Number of requests processed</li>
%%   <li>Number of items that were inserted into a queue</li>
%%   <li>Total amount of data a system has processed</li>
%% </ul>
%%
%% Use the
%% <a href="https://prometheus.io/docs/querying/functions/#rate()">rate()</a>/<a
%% href="https://prometheus.io/docs/querying/functions/#irate()">irate()</a>
%% functions in Prometheus to calculate the rate of increase of a Counter.
%% By convention, the names of Counters are suffixed by `_total'.
%%
%% @end

-module(prometheus_counter).

%%% metric
-export([new/1,
         new/2,
         declare/1,
         declare/2,
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
-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

%%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         start_link/0]).

-import(prometheus_model_helpers, [create_mf/5,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).
-behaviour(gen_server).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_COUNTER_TABLE).
-define(SUM_POS, 2).

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
  ?DEPRECATED("prometheus_counter:new/2", "prometheus_counter:new/1"
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
  ?DEPRECATED("prometheus_counter:declare/2", "prometheus_counter:declare/1"
              " with registry key"),
  declare([{registry, Registry} | Spec]).

%% @equiv inc(default, Name, [], 1)
inc(Name) ->
  inc(default, Name, [], 1).

inc(Name, LabelValues) when is_list(LabelValues)->
  inc(default, Name, LabelValues, 1);
inc(Name, Value) when is_number(Value) ->
  inc(default, Name, [], Value).

%% @equiv inc(default, Name, LabelValues, Value)
inc(Name, LabelValues, Value) ->
  inc(default, Name, LabelValues, Value).

inc(_Registry, _Name, _LabelValues, Value) when Value < 0 ->
  erlang:error({invalid_value, Value,
                "Counters accept only non-negative values"});
inc(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, {Registry, Name, LabelValues}, {?SUM_POS, Value})
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun inc/4)
  end,
  ok;
inc(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "inc accepts only integers"}).

%% @equiv dinc(default, Name, [], 1)
dinc(Name) ->
  dinc(default, Name, [], 1).

dinc(Name, LabelValues) when is_list(LabelValues)->
  dinc(default, Name, LabelValues, 1);
dinc(Name, Value) when is_number(Value) ->
  dinc(default, Name, [], Value).

%% @equiv dinc(default, Name, LabelValues, Value)
dinc(Name, LabelValues, Value) ->
  dinc(default, Name, LabelValues, Value).

dinc(_Registry, _Name, _LabelValues, Value) when Value < 0 ->
  erlang:error({invalid_value, Value,
                "Counters accept only non-negative values"});
dinc(Registry, Name, LabelValues, Value) when is_number(Value) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  gen_server:cast(prometheus_counter,
                  {inc, {Registry, Name, LabelValues, Value}}),
  ok;
dinc(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "dinc accepts only numbers"}).

%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  ets:update_element(?TABLE, {Registry, Name, LabelValues}, {?SUM_POS, 0}).

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

%% @private
deregister_cleanup(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  true = ets:match_delete(?TABLE, {{Registry, '_', '_'}, '_'}),
  ok.

%% @private
collect_mf(Callback, Registry) ->
  [Callback(create_counter(Name, Help, {Labels, Registry})) ||
    [Name, Labels, Help, _] <- prometheus_metric:metrics(?TABLE, Registry)].

%% @private
collect_metrics(Name, {Labels, Registry}) ->
  [counter_metric(lists:zip(Labels, LabelValues), Value) ||
    [LabelValues, Value] <- ets:match(?TABLE, {{Registry, Name, '$1'}, '$2'})].

%%====================================================================
%% Gen_server API
%%====================================================================

%% @private
init(_Args) ->
  {ok, []}.

%% @private
handle_call(_Call, _From, State) ->
  {noreply, State}.

%% @private
handle_cast({inc, {Registry, Name, LabelValues, Value}}, State) ->
  dinc_impl(Registry, Name, LabelValues, Value),
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @private
start_link() ->
  gen_server:start_link({local, prometheus_counter},
                        prometheus_counter, [], []).

%%====================================================================
%% Private Parts
%%====================================================================

dinc_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, OldValue}] ->
      ets:update_element(?TABLE, {Registry, Name, LabelValues},
                         {?SUM_POS, Value + OldValue});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dinc_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {{Registry, Name, LabelValues}, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).
