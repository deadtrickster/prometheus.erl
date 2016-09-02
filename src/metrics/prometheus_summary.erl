%% @doc
%% Summary metric, to track the size of events.
%%
%% Example use cases for Summaries:
%% <ul>
%%   <li>Response latency</li>
%%   <li>Request size</li>
%% </ul>
%% @end

-module(prometheus_summary).

%%% metric
-export([new/1,
         new/2,
         declare/1,
         declare/2,
         observe/2,
         observe/3,
         observe/4,
         dobserve/2,
         dobserve/3,
         dobserve/4,
         observe_duration/2,
         observe_duration/3,
         observe_duration/4,
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
                                   counter_metric/2,
                                   summary_metric/3]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).
-behaviour(gen_server).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_SUMMARY_TABLE).
-define(SUM_POS, 3).
-define(COUNTER_POS, 2).

%%====================================================================
%% Metric API
%%====================================================================

new(Spec) ->
  {Registry, Name, Labels, Help} = parse_summary_spec(Spec),
  prometheus_registry:register_collector(Registry, ?MODULE),
  prometheus_metric:insert_new_mf(?TABLE, Registry, Name, Labels, Help).

%% @deprecated Please use {@link new/1} with registry
%% key instead.
new(Spec, Registry) ->
  ?DEPRECATED("prometheus_summary:new/2", "prometheus_summary:new/1"
              " with registry key"),
  new([{registry, Registry} | Spec]).

declare(Spec) ->
  {Registry, Name, Labels, Help} = parse_summary_spec(Spec),
  prometheus_registry:register_collector(Registry, ?MODULE),
  prometheus_metric:insert_mf(?TABLE, Registry, Name, Labels, Help).

%% @deprecated Please use {@link declare/1} with registry
%% key instead.
declare(Spec, Registry) ->
  ?DEPRECATED("prometheus_summary:declare/2", "prometheus_summary:declare/1"
              " with registry key"),
  declare([{registry, Registry} | Spec]).

%% @equiv observe(default, Name, [], Value)
observe(Name, Value) ->
  observe(default, Name, [], Value).

%% @equiv observe(default, Name, LabelValues, Value)
observe(Name, LabelValues, Value) ->
  observe(default, Name, LabelValues, Value).

observe(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                       [{?COUNTER_POS, 1}, {?SUM_POS, Value}])
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun observe/4)
  end,
  ok;
observe(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "observe accepts only integers"}).

%% @equiv dobserve(default, Name, [], Value)
dobserve(Name, Value) ->
  dobserve(default, Name, [], Value).

%% @equiv dobserve(default, Name, LabelValues, Value)
dobserve(Name, LabelValues, Value) ->
  dobserve(default, Name, LabelValues, Value).

dobserve(Registry, Name, LabelValues, Value) when is_number(Value) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  gen_server:cast(?MODULE,
                  {observe, {Registry, Name, LabelValues, Value}}),
  ok;
dobserve(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "dobserve accepts only numbers"}).

%% @equiv observe_duration(default, Name, [], Fun)
observe_duration(Name, Fun) ->
  observe_duration(default, Name, [], Fun).

%% @equiv observe_duration(default, Name, LabelValues, Fun)
observe_duration(Name, LabelValues, Fun) ->
  observe_duration(default, Name, LabelValues, Fun).

observe_duration(Registry, Name, LabelValues, Fun) when is_function(Fun)->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  prometheus_misc:observe_duration(Registry, ?MODULE, Name, LabelValues, Fun);
observe_duration(_Regsitry, _Name, _LabelValues, Fun) ->
  erlang:error({invalid_value, Fun, "observe_duration accepts only functions"}).


%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  ets:update_element(?TABLE, {Registry, Name, LabelValues},
                     [{?COUNTER_POS, 0}, {?SUM_POS, 0}]).

%% @equiv value(default, Name, [])
value(Name) ->
  value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case  ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, Count, Sum}] -> {Count, Sum};
    [] -> undefined
  end.

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  true = ets:match_delete(?TABLE, {{Registry, '_', '_'}, '_', '_'}),
  ok.

%% @private
collect_mf(Registry, Callback) ->
  [Callback(create_summary(Name, Help, {Labels, Registry})) ||
    [Name, Labels, Help, _] <- prometheus_metric:metrics(?TABLE, Registry)],
  ok.

%% @private
collect_metrics(Name, {Labels, Registry}) ->
  [summary_metric(lists:zip(Labels, LabelValues), Count, Sum) ||
    [LabelValues, Count, Sum] <- ets:match(?TABLE, {{Registry, Name, '$1'},
                                                    '$2', '$3'})].

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
handle_cast({observe, {Registry, Name, LabelValues, Value}}, State) ->
  dobserve_impl(Registry, Name, LabelValues, Value),
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
  gen_server:start_link({local, prometheus_summary},
                        prometheus_summary, [], []).

%%====================================================================
%% Private Parts
%%====================================================================

parse_summary_spec(Spec) ->
  {Registry, Name, Labels, Help} =
    prometheus_metric:extract_common_params(Spec),
  validate_summary_labels(Labels),
  {Registry, Name, Labels, Help}.

validate_summary_labels(Labels) ->
  [raise_error_if_quantile_label_found(Label) || Label <- Labels].

raise_error_if_quantile_label_found("quantile") ->
  erlang:error({invalid_metric_label_name, "quantile",
                "summary cannot have a label named \"quantile\""});
raise_error_if_quantile_label_found(Label) ->
  Label.

dobserve_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [Metric] ->
      ets:update_element(?TABLE, {Registry, Name, LabelValues},
                         {?SUM_POS, sum(Metric) + Value}),
      ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                         {?COUNTER_POS, 1});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dobserve_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {{Registry, Name, LabelValues}, 1, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

sum(Metric) ->
  element(?SUM_POS, Metric).

create_summary(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, summary, ?MODULE, Data).
