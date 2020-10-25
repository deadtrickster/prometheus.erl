%% @doc
%% Summary metric, to track the size of events.
%%
%% Example use cases for Summaries:
%%   - Response latency;
%%   - Request size;
%%   - Response size.
%%
%% Example:
%% <pre lang="erlang">
%% -module(my_proxy_instrumenter).
%%
%% setup() ->
%%   prometheus_summary:declare([{name, request_size_bytes},
%%                               {help, "Request size in bytes."}]),
%%   prometheus_summary:declare([{name, response_size_bytes},
%%                               {help, "Response size in bytes."}]).
%%
%% observe_request(Size) ->
%%   prometheus_summary:observe(request_size_bytes, Size).
%%
%% observe_response(Size) ->
%%   prometheus_summary:observe(response_size_bytes, Size).
%% </pre>
%% @end

-module(prometheus_summary).

%%% metric
-export([new/1,
         declare/1,
         deregister/1,
         deregister/2,
         set_default/2,
         observe/2,
         observe/3,
         observe/4,
         observe_duration/2,
         observe_duration/3,
         observe_duration/4,
         remove/1,
         remove/2,
         remove/3,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3,
         values/2]).

%%% collector
-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_SUMMARY_TABLE).
-define(ISUM_POS, 3).
-define(FSUM_POS, 4).
-define(COUNTER_POS, 2).
-define(WIDTH, 16).

%%====================================================================
%% Metric API
%%====================================================================

%% @doc Creates a summary using `Spec'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Soec' key
%% is missing.<br/>
%% Raises `{invalid_metric_name, Name, Message}' error if metric `Name'
%% is invalid.<br/>
%% Raises `{invalid_metric_help, Help, Message}' error if metric `Help'
%% is invalid.<br/>
%% Raises `{invalid_metric_labels, Labels, Message}' error if `Labels'
%% isn't a list.<br/>
%% Raises `{invalid_label_name, Name, Message}' error if `Name' isn't a valid
%% label name.<br/>
%% Raises `{invalid_value_error, Value, Message}' error if `duration_unit' is
%% unknown or doesn't match metric name.<br/>
%% Raises `{mf_already_exists, {Registry, Name}, Message}' error if a summary
%% with the same `Spec' already exists.
%% @end
new(Spec) ->
  validate_summary_spec(Spec),
  prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

%% @doc Creates a summary using `Spec'.
%% If a summary with the same `Spec' exists returns `false'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Soec' key
%% is missing.<br/>
%% Raises `{invalid_metric_name, Name, Message}' error if metric `Name'
%% is invalid.<br/>
%% Raises `{invalid_metric_help, Help, Message}' error if metric `Help'
%% is invalid.<br/>
%% Raises `{invalid_metric_labels, Labels, Message}' error if `Labels'
%% isn't a list.<br/>
%% Raises `{invalid_label_name, Name, Message}' error if `Name' isn't a valid
%% label name.<br/>
%% Raises `{invalid_value_error, Value, MessagE}' error if `duration_unit' is
%% unknown or doesn't match metric name.<br/>
%% @end
declare(Spec) ->
  Spec1 = validate_summary_spec(Spec),
  prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec1).

%% @equiv deregister(default, Name)
deregister(Name) ->
  deregister(default, Name).

%% @doc
%% Removes all summary series with name `Name' and
%% removes Metric Family from `Registry'.
%%
%% After this call new/1 for `Name' and `Registry' will succeed.
%%
%% Returns `{true, _}' if `Name' was a registered summary.
%% Otherwise returns `{false, _}'.
%% @end
deregister(Registry, Name) ->
  MFR = prometheus_metric:deregister_mf(?TABLE, Registry, Name),
  NumDeleted = ets:select_delete(?TABLE, deregister_select(Registry, Name)),
  {MFR, NumDeleted > 0}.

%% @private
set_default(Registry, Name) ->
  ets:insert_new(?TABLE, {key(Registry, Name, []), 0, 0, 0}).

%% @equiv observe(default, Name, [], Value)
observe(Name, Value) ->
  observe(default, Name, [], Value).

%% @equiv observe(default, Name, LabelValues, Value)
observe(Name, LabelValues, Value) ->
  observe(default, Name, LabelValues, Value).

%% @doc Observes the given `Value'.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't an integer.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if summary with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
observe(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, key(Registry, Name, LabelValues),
                       [{?COUNTER_POS, 1}, {?ISUM_POS, Value}])
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun observe/4)
  end,
  ok;
observe(Registry, Name, LabelValues, Value) when is_number(Value) ->
  Key = key(Registry, Name, LabelValues),
  case
    ets:select_replace(?TABLE,
                       [{{Key, '$1', '$2', '$3'},
                         [],
                         [{{{Key}, {'+', '$1', 1}, '$2', {'+', '$3', Value}}}]}]) of
    0 ->
      insert_metric(Registry, Name, LabelValues, Value, fun observe/4);
    1 ->
      ok
  end;
observe(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "observe accepts only numbers"}).

%% @equiv observe_duration(default, Name, [], Fun)
observe_duration(Name, Fun) ->
  observe_duration(default, Name, [], Fun).

%% @equiv observe_duration(default, Name, LabelValues, Fun)
observe_duration(Name, LabelValues, Fun) ->
  observe_duration(default, Name, LabelValues, Fun).

%% @doc Tracks the amount of time spent executing `Fun'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if summary with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% Raises `{invalid_value, Value, Message}' if `Fun'
%% isn't a function.<br/>
%% @end
observe_duration(Registry, Name, LabelValues, Fun) when is_function(Fun)->
  Start = erlang:monotonic_time(),
  try
    Fun()
  after
    observe(Registry, Name, LabelValues, erlang:monotonic_time() - Start)
  end;
observe_duration(_Regsitry, _Name, _LabelValues, Fun) ->
  erlang:error({invalid_value, Fun, "observe_duration accepts only functions"}).

%% @equiv remove(default, Name, [])
remove(Name) ->
  remove(default, Name, []).

%% @equiv remove(default, Name, LabelValues)
remove(Name, LabelValues) ->
  remove(default, Name, LabelValues).

%% @doc Removes summary series identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if summary with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
remove(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case lists:flatten([ets:take(?TABLE,
                               {Registry, Name, LabelValues, Scheduler})
                      || Scheduler <- schedulers_seq()]) of
    [] -> false;
    _ -> true
  end.

%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

%% @doc Resets the value of the summary identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if summary with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case lists:usort([ets:update_element(?TABLE,
                                       {Registry, Name, LabelValues, Scheduler},
                                       [{?COUNTER_POS, 0}, {?ISUM_POS, 0}, {?FSUM_POS, 0}])
                    || Scheduler <- schedulers_seq()]) of
    [_, _] -> true;
    [true] -> true;
    _ -> false
  end.

%% @equiv value(default, Name, [])
value(Name) ->
  value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
value(Name, LabelValues) ->
  value(default, Name, LabelValues).

%% @doc Returns the value of the summary identified by `Registry', `Name'
%% and `LabelValues'. If there is no summary for `LabelValues',
%% returns `undefined'.
%%
%% If duration unit set, sum will be converted to the duration unit.
%% {@link prometheus_time. Read more here.}
%%
%% Raises `{unknown_metric, Registry, Name}' error if summary named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
value(Registry, Name, LabelValues) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  DU = prometheus_metric:mf_duration_unit(MF),

  case ets:select(?TABLE, [{{{Registry, Name, LabelValues, '_'}, '$1', '$2', '$3'},
                            [],
                            ['$$']}]) of
    [] -> undefined;
    Values -> {Count, Sum} = reduce_values(Values),
              {Count,  prometheus_time:maybe_convert_to_du(DU, Sum)}
  end.

values(Registry, Name) ->
  case prometheus_metric:check_mf_exists(?TABLE, Registry, Name) of
    false -> [];
    MF ->
      DU = prometheus_metric:mf_duration_unit(MF),
      Labels = prometheus_metric:mf_labels(MF),
      MFValues = load_all_values(Registry, Name),
      ReducedMap = lists:foldl(
        fun([L, C, IS, FS], ResAcc) ->
          {PrevCount, PrevSum} = maps:get(L, ResAcc, {0, 0}),
          ResAcc#{L => {PrevCount + C, PrevSum + IS + FS}}
        end,
      #{},
      MFValues),
      ReducedMapList = lists:sort(maps:to_list(ReducedMap)),
      lists:foldr(
        fun({LabelValues, {Count, Sum}}, Acc) ->
          [{lists:zip(Labels, LabelValues), Count,
          prometheus_time:maybe_convert_to_du(DU, Sum)} | Acc]
        end,
        [],
      ReducedMapList)
  end.

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  true = ets:match_delete(?TABLE, {{Registry, '_', '_', '_'}, '_', '_', '_'}),
  ok.

%% @private
collect_mf(Registry, Callback) ->
  [Callback(create_summary(Name, Help, {CLabels, Labels, Registry, DU})) ||
    [Name, {Labels, Help}, CLabels, DU, _] <- prometheus_metric:metrics(?TABLE,
                                                                        Registry)],
  ok.

%% @private
collect_metrics(Name, {CLabels, Labels, Registry, DU}) ->
  MFValues = load_all_values(Registry, Name),
  ReducedMap = lists:foldl(
        fun([L, C, IS, FS], ResAcc) ->
          {PrevCount, PrevSum} = maps:get(L, ResAcc, {0, 0}),
          ResAcc#{L => {PrevCount + C, PrevSum + IS + FS}}
        end,
      #{},
      MFValues),
  ReducedMapList = lists:sort(maps:to_list(ReducedMap)),
  lists:foldr(
    fun({LabelValues, {Count, Sum}}, Acc) ->
      [prometheus_model_helpers:summary_metric(
      CLabels ++ lists:zip(Labels, LabelValues), Count,
      prometheus_time:maybe_convert_to_du(DU, Sum)) | Acc]
    end,
    [],
  ReducedMapList).

%%====================================================================
%% Private Parts
%%====================================================================

deregister_select(Registry, Name) ->
  [{{{Registry, Name, '_', '_'}, '_', '_', '_'}, [], [true]}].

validate_summary_spec(Spec) ->
  Labels = prometheus_metric_spec:labels(Spec),
  validate_summary_labels(Labels),
  Spec.

validate_summary_labels(Labels) ->
  [raise_error_if_quantile_label_found(Label) || Label <- Labels].

raise_error_if_quantile_label_found("quantile") ->
  erlang:error({invalid_metric_label_name, "quantile",
                "summary cannot have a label named \"quantile\""});
raise_error_if_quantile_label_found(Label) ->
  Label.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {key(Registry, Name, LabelValues), 1, 0, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

load_all_values(Registry, Name) ->
  ets:match(?TABLE, {{Registry, Name, '$1', '_'}, '$2', '$3', '$4'}).

schedulers_seq() ->
  lists:seq(0, ?WIDTH-1).

key(Registry, Name, LabelValues) ->
  X = erlang:system_info(scheduler_id),
  Rnd = X band (?WIDTH-1),
  {Registry, Name, LabelValues, Rnd}.

reduce_values(Values) ->
  {lists:sum([C || [C, _, _] <- Values]),
   lists:sum([IS + FS || [_, IS, FS] <- Values])}.

create_summary(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, summary, ?MODULE, Data).
