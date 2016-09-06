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
         inc/1,
         inc/2,
         inc/3,
         inc/4,
         dinc/1,
         dinc/2,
         dinc/3,
         dinc/4,
         dec/1,
         dec/2,
         dec/3,
         dec/4,
         ddec/1,
         ddec/2,
         ddec/3,
         ddec/4,
         set_to_current_time/1,
         set_to_current_time/2,
         set_to_current_time/3,
         track_inprogress/2,
         track_inprogress/3,
         track_inprogress/4,
         set_duration/2,
         set_duration/3,
         set_duration/4,
         remove/1,
         remove/2,
         remove/3,
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

-define(TABLE, ?PROMETHEUS_GAUGE_TABLE).
-define(GAUGE_POS, 2).

%%====================================================================
%% Metric API
%%====================================================================

new(Spec) ->
  prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

%% @deprecated Please use {@link new/1} with registry
%% key instead.
new(Spec, Registry) ->
  ?DEPRECATED("prometheus_gauge:new/2", "prometheus_gauge:new/1"
              " with registry key"),
  new([{registry, Registry} | Spec]).

declare(Spec) ->
  prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec).

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

set(Registry, Name, LabelValues, Value) when is_number(Value) orelse
                                             Value == undefined ->
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

inc(Name) ->
  inc(default, Name, [], 1).

inc(Name, LabelValues) when is_list(LabelValues)->
  inc(default, Name, LabelValues, 1);
inc(Name, Value) ->
  inc(default, Name, [], Value).

%% @equiv inc(default, Name, LabelValues, Value)
inc(Name, LabelValues, Value) ->
  inc(default, Name, LabelValues, Value).

inc(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                       {?GAUGE_POS, Value})
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun inc/4)
  end,
  ok;
inc(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "inc accepts only integers"}).

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

dinc(Registry, Name, LabelValues, Value) when is_number(Value) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  gen_server:cast(?MODULE,
                  {inc, {Registry, Name, LabelValues, Value}}),
  ok;
dinc(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "dinc accepts only numbers"}).

dec(Name) ->
  inc(default, Name, [], -1).

dec(Name, LabelValues) when is_list(LabelValues)->
  inc(default, Name, LabelValues, -1);
dec(Name, Value)  when is_integer(Value) ->
  inc(default, Name, [], -1*Value);
dec(_Name, Value) ->
  erlang:error({invalid_value, Value,
                "dec accepts only integers"}).

dec(Name, LabelValues, Value) when is_integer(Value) ->
  inc(default, Name, LabelValues, -1*Value);
dec(_Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "dec accepts only integers"}).

dec(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  inc(Registry, Name, LabelValues, -1*Value);
dec(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "dec accepts only integers"}).

ddec(Name) ->
  dinc(default, Name, [], -1).

ddec(Name, LabelValues) when is_list(LabelValues)->
  dinc(default, Name, LabelValues, -1);
ddec(Name, Value)  when is_number(Value) ->
  dinc(default, Name, [], -1*Value);
ddec(_Name, Value) ->
  erlang:error({invalid_value, Value,
                "ddec accepts only numbers"}).

ddec(Name, LabelValues, Value) when is_number(Value) ->
  dinc(default, Name, LabelValues, -1*Value);
ddec(_Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "ddec accepts only numbers"}).

ddec(Registry, Name, LabelValues, Value) when is_number(Value) ->
  dinc(Registry, Name, LabelValues, -1*Value);
ddec(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "ddec accepts only numbers"}).

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

track_inprogress(Registry, Name, LabelValues, Fun) when is_function(Fun) ->
  inc(Registry, Name, LabelValues, 1),
  try
    Fun()
  after
    dec(Registry, Name, LabelValues, 1)
  end;
track_inprogress(_Registry, _Name, _LabelValues, Fun) ->
  erlang:error({invalid_value, Fun, "track_inprogress accepts only functions"}).

%% @equiv set_duration(default, Name, [], Fun)
set_duration(Name, Fun) ->
  set_duration(default, Name, [], Fun).

%% @equiv set_duration(default, Name, LabelValues, Fun)
set_duration(Name, LabelValues, Fun) ->
  set_duration(default, Name, LabelValues, Fun).

set_duration(Registry, Name, LabelValues, Fun) when is_function(Fun) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  prometheus_misc:set_duration(Registry, ?MODULE, Name, LabelValues, Fun);
set_duration(_Registry, _Name, _LabelValues, Fun) ->
  erlang:error({invalid_value, Fun, "set_duration accepts only functions"}).

%% @equiv remove(default, Name, [])
remove(Name) ->
  remove(default, Name, []).

%% @equiv remove(default, Name, LabelValues)
remove(Name, LabelValues) ->
  remove(default, Name, LabelValues).

remove(Registry, Name, LabelValues) ->
  prometheus_metric:remove_labels(?TABLE, Registry, Name, LabelValues).

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

collect_mf(Registry, Callback) ->
  [Callback(create_gauge(Name, Help, {Labels, Registry})) ||
    [Name, {Labels, Help}, _, _, _] <- prometheus_metric:metrics(?TABLE,
                                                                  Registry)],
  ok.

collect_metrics(Name, {Labels, Registry}) ->
  [gauge_metric(lists:zip(Labels, LabelValues), Value) ||
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
  gen_server:start_link({local, prometheus_gauge},
                        prometheus_gauge, [], []).


%%====================================================================
%% Private Parts
%%====================================================================

dinc_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, OldValue}] ->
      ets:update_element(?TABLE, {Registry, Name, LabelValues},
                         {?GAUGE_POS, Value + OldValue});
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

create_gauge(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).
