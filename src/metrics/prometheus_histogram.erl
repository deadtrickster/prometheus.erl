-module(prometheus_histogram).

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
         value/3,
         buckets/1,
         buckets/2,
         buckets/3,
         default_buckets/0,
         linear_buckets/3,
         exponential_buckets/3]
       ).

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
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2,
                                   summary_metric/3,
                                   histogram_metric/4]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).


%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_HISTOGRAM_TABLE).
-define(BUCKETS_POS, 2).
-define(BUCKETS_START, 3).
-define(DEPRECATED(Old, New),
        error_logger:warning_msg(Old " is deprecated and will soon be removed. "
                                 "Please use " New " instead.~n")).

%%====================================================================
%% Metric API
%%====================================================================

%% @equiv new(Spec, default)
new(Spec) ->
  new(Spec, default).

new(Spec, Registry) ->
  {Name, Labels, Help, Buckets} = parse_histogram_spec(Spec),
  prometheus_collector:register(?MODULE, Registry),
  prometheus_metric:insert_new_mf(?TABLE, Registry,
                                  Name, Labels, Help, Buckets).

declare(Spec) ->
  declare(Spec, default).

declare(Spec, Registry) ->
  {Name, Labels, Help, Buckets} = parse_histogram_spec(Spec),
  prometheus_collector:register(?MODULE, Registry),
  prometheus_metric:insert_mf(?TABLE, Registry, Name, Labels, Help, Buckets).

observe(Name, Value) ->
  observe(default, Name, [], Value).

observe(Name, LabelValues, Value) ->
  observe(default, Name, LabelValues, Value).

observe(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [Metric] ->
      {BucketPosition, SumPosition} =
        calculate_histogram_update_positions(Metric, Value),
      ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                         [{BucketPosition, 1}, {SumPosition, Value}]);
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun observe/4)
  end,
  ok;
observe(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "observe accepts only integers"}).

dobserve(Name, Value) ->
  dobserve(default, Name, [], Value).

dobserve(Name, LabelValues, Value) ->
  dobserve(default, Name, LabelValues, Value).

dobserve(Registry, Name, LabelValues, Value) when is_number(Value) ->
  gen_server:cast(prometheus_histogram,
                  {observe, {Registry, Name, LabelValues, Value}}),
  ok;
dobserve(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "dobserve accepts only numbers"}).

observe_duration(Name, Fun) ->
  prometheus_misc:observe_duration(default, ?MODULE, Name, [], Fun).

observe_duration(Name, LabelValues, Fun) ->
  prometheus_misc:observe_duration(default, ?MODULE, Name, LabelValues, Fun).

observe_duration(Registry, Name, LabelValues, Fun) -> %% FIXME: args order
  prometheus_misc:observe_duration(Registry, ?MODULE, Name, LabelValues, Fun).

%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  Buckets = prometheus_metric:mf_data(MF),
  UpdateSpec = generate_update_spec(?BUCKETS_START, length(Buckets)),
  ets:update_element(?TABLE, {Registry, Name, LabelValues}, UpdateSpec).

%% @equiv value(default, Name, [])
value(Name) ->
  value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [Metric] = ets:lookup(?TABLE, {Registry, Name, LabelValues}),
  {buckets_counters(Metric), sum(Metric)}.

%% @equiv buckets(default, Name, [])
buckets(Name) ->
  buckets(default, Name, []).

%% @equiv buckets(default, Name, LabelValues)
buckets(Name, LabelValues) ->
  buckets(default, Name, LabelValues).

buckets(Registry, Name, LabelValues) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  prometheus_metric:mf_data(MF).

default_buckets () ->
  prometheus_buckets:default().

linear_buckets(Start, Step, Count) ->
  prometheus_buckets:linear(Start, Step, Count).

exponential_buckets(Start, Factor, Count) ->
  prometheus_buckets:exponential(Start, Factor, Count).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(Registry) ->
  [delete_metrics(Registry, Buckets)
   || [_, _, _, Buckets] <- prometheus_metric:metrics(?TABLE, Registry)],
  true = prometheus_metric:deregister_mf(?TABLE, Registry),
  ok.

collect_mf(Callback, Registry) ->
  [Callback(create_histogram(Name, Help, {Labels, Registry, Buckets})) ||
    [Name, Labels, Help, Buckets]
      <- prometheus_metric:metrics(?TABLE, Registry)].

collect_metrics(Name, {Labels, Registry, Buckets}) ->
  BoundPlaceholders = gen_query_bound_placeholders(Buckets),
  SumPlaceholder = gen_query_placeholder(sum_position(Buckets)),
  QuerySpec =
    [{Registry, Name, '$1'}, '$2']
    ++ BoundPlaceholders
    ++ [SumPlaceholder],
  [create_histogram_metric(Labels, Value) ||
    Value <- ets:match(?TABLE, list_to_tuple(QuerySpec))].

%%====================================================================
%% Gen_server API
%%====================================================================

start_link() ->
  gen_server:start_link({local, prometheus_histogram},
                        prometheus_histogram, [], []).

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

%%====================================================================
%% Private Parts
%%====================================================================

parse_histogram_spec(Spec) ->
  {Name, Labels, Help} = prometheus_metric:extract_common_params(Spec),
  validate_histogram_labels(Labels),
  Buckets =
    case prometheus_metric_spec:get_value(bounds, Spec, undefined) of
      undefined ->
        prometheus_metric_spec:get_value(buckets, Spec, default_buckets());
      Bounds ->
        ?DEPRECATED("Bounds config for Prometheus histograms", "buckets"),
        Bounds
    end,
  {Name, Labels, Help, validate_histogram_buckets(Buckets)}.

validate_histogram_labels(Labels) ->
  [raise_error_if_le_label_found(Label) || Label <- Labels].

raise_error_if_le_label_found("le") ->
  erlang:error({invalid_metric_label_name, "le",
                "histogram cannot have a label named \"le\""});
raise_error_if_le_label_found(Label) ->
  Label.

validate_histogram_buckets([]) ->
  erlang:error({histogram_no_buckets, []});
validate_histogram_buckets(undefined) ->
  erlang:error({histogram_no_buckets, undefined});
validate_histogram_buckets(default) ->
  default_buckets();
validate_histogram_buckets({linear, Start, Step, Count}) ->
  linear_buckets(Start, Step, Count) ++ [infinity];
validate_histogram_buckets({exponential, Start, Factor, Count}) ->
  exponential_buckets(Start, Factor, Count) ++ [infinity];
validate_histogram_buckets(RawBuckets) when is_list(RawBuckets) ->
  Buckets = lists:map(fun validate_histogram_bound/1, RawBuckets),
  case lists:sort(Buckets) of
    Buckets ->
      Buckets ++ [infinity];
    _ ->
      erlang:error({histogram_invalid_buckets, Buckets, "Buckets not sorted"})
  end;
validate_histogram_buckets(Buckets) ->
  erlang:error({histogram_invalid_buckets, Buckets}). %% FIXME: why no message?

validate_histogram_bound(Bound) when is_number(Bound) ->
  Bound;
validate_histogram_bound(Bound) ->
  erlang:error({histogram_invalid_bound, Bound}).

dobserve_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [Metric] ->
      {BucketPosition, SumPosition} =
        calculate_histogram_update_positions(Metric, Value),
      ets:update_element(?TABLE, {Registry, Name, LabelValues},
                         {SumPosition, sum(Metric) + Value}),
      ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                         {BucketPosition, 1});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dobserve_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, CB) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  MFBuckets = prometheus_metric:mf_data(MF),
  BoundCounters = lists:duplicate(length(MFBuckets), 0),
  MetricSpec =
    [{Registry, Name, LabelValues}, MFBuckets]
    ++ BoundCounters
    ++ [0],
  ets:insert(?TABLE, list_to_tuple(MetricSpec)),
  CB(Registry, Name, LabelValues, Value).

calculate_histogram_update_positions(Metric, Value) ->
  Buckets = metric_buckets(Metric),
  BucketPosition = ?BUCKETS_POS + position(Buckets, fun(Bound) ->
                                                        Value =< Bound
                                                    end),
  SumPosition = sum_position(Metric),
  {BucketPosition, SumPosition}.

generate_update_spec(BucketsStart, BucketsCount) ->
  [{Index, 0} ||
    Index <- lists:seq(BucketsStart, ?BUCKETS_START + BucketsCount)].

gen_query_placeholder(Index) ->
  list_to_atom("$" ++ integer_to_list(Index)).

gen_query_bound_placeholders(Buckets) ->
  [gen_query_placeholder(Index) ||
    Index <- lists:seq(?BUCKETS_START, ?BUCKETS_POS + length(Buckets))].

augment_counters([]) ->
  0;
augment_counters([Start | Counters]) ->
  augment_counters(Counters, [Start], Start).

augment_counters([], LAcc, _CAcc) ->
  LAcc;
augment_counters([Counter | Counters], LAcc, CAcc) ->
  augment_counters(Counters, LAcc ++ [CAcc + Counter], CAcc + Counter).

metric_buckets(Metric) ->
  element(?BUCKETS_POS, Metric).

buckets_counters(Metric) ->
  sub_tuple_to_list(Metric, ?BUCKETS_START,
                    ?BUCKETS_START + length(metric_buckets(Metric))).

sum_position(Metric) when is_tuple(Metric) ->
  ?BUCKETS_START + length(metric_buckets(Metric));
sum_position(Buckets) when is_list(Buckets) ->
  ?BUCKETS_START + length(Buckets).

sum(Metric) ->
  element(sum_position(Metric), Metric).

create_histogram_metric(Labels, [LabelValues, Buckets | Stat]) ->
  BoundValues = lists:sublist(Stat, 1, length(Buckets)),
  BCounters = augment_counters(BoundValues),
  Buckets1 = lists:zipwith(fun(Bound, BCounter) ->
                               {Bound, BCounter}
                           end,
                           Buckets, BCounters),
  histogram_metric(lists:zip(Labels, LabelValues),
                   Buckets1, lists:last(BCounters), lists:last(Stat)).

delete_metrics(Registry, Buckets) ->
  BoundCounters = lists:duplicate(length(Buckets), '_'),
  MetricSpec = [{Registry, '_', '_'}, '_'] ++ BoundCounters ++ ['_'],
  ets:match_delete(?TABLE, list_to_tuple(MetricSpec)).

sub_tuple_to_list(Tuple, Pos, Size) when Pos < Size ->
  [element(Pos, Tuple) | sub_tuple_to_list(Tuple, Pos + 1, Size)];
sub_tuple_to_list(_Tuple, _Pos, _Size) -> [].

position([], _Pred) ->
  0;
position(List, Pred) ->
  position(List, Pred, 1).

position([], _Pred, _Pos) ->
  0;
position([H|L], Pred, Pos) ->
  case Pred(H) of
    true ->
      Pos;
    false ->
      position(L, Pred, Pos + 1)
  end.

create_histogram(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, histogram, ?MODULE, Data).
