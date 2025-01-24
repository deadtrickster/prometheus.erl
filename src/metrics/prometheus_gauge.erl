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
%%
%% Example:
%% <pre lang="erlang">
%% -module(my_pool_instrumenter).
%%
%% -export([setup/0,
%%          set_size/1]).
%%
%% setup() ->
%%   prometheus_gauge:declare([{name, my_pool_size},
%%                             {help, "Pool size."}]),
%%   prometheus_gauge:declare([{name, my_pool_checked_out},
%%                             {help, "Number of checked out sockets"}]).
%%
%% set_size(Size) ->
%%   prometheus_gauge:set(my_pool_size, Size)
%%
%% track_checked_out_sockets(CheckoutFun) ->
%%   prometheus_gauge:track_inprogress(my_pool_checked_out, CheckoutFun)..
%% </pre>
%% @end
-module(prometheus_gauge).

%%% metric
-export([new/1,
         declare/1,
         deregister/1,
         deregister/2,
         set_default/2,
         set/2,
         set/3,
         set/4,
         inc/1,
         inc/2,
         inc/3,
         inc/4,
         dec/1,
         dec/2,
         dec/3,
         dec/4,
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
         match_remove/1,
         match_remove/3,
         match_remove/4,
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

-define(TABLE, ?PROMETHEUS_GAUGE_TABLE).
-define(IGAUGE_POS, 2).
-define(FGAUGE_POS, 3).

%%====================================================================
%% Metric API
%%====================================================================

%% @doc Creates a gauge using `Spec'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Spec' key
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
%% Raises `{mf_already_exists, {Registry, Name}, Message}' error if a gauge
%% with the same `Spec' already exists.
%% @end
new(Spec) ->
  prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

%% @doc Creates a gauge using `Spec'.
%% If a gauge with the same `Spec' exists returns `false'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Spec' key
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
  prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec).

%% @equiv deregister(default, Name)
deregister(Name) ->
  deregister(default, Name).

%% @doc
%% Removes all gauge series with name `Name' and
%% removes Metric Family from `Registry'.
%%
%% After this call new/1 for `Name' and `Registry' will succeed.
%%
%% Returns `{true, _}' if `Name' was a registered gauge.
%% Otherwise returns `{false, _}'.
%% @end
deregister(Registry, Name) ->
  MFR = prometheus_metric:deregister_mf(?TABLE, Registry, Name),
  NumDeleted = ets:select_delete(?TABLE, deregister_select(Registry, Name)),
  {MFR, NumDeleted > 0}.

%% @private
set_default(Registry, Name) ->
  ets:insert_new(?TABLE, {{Registry, Name, []}, 0, 0}).

%% @equiv set(default, Name, [], Value)
set(Name, Value) ->
  set(default, Name, [], Value).

%% @equiv set(default, Name, LabelValues, Value)
set(Name, LabelValues, Value) ->
  set(default, Name, LabelValues, Value).

%% @doc Sets the gauge identified by `Registry', `Name'
%% and `LabelValues' to `Value'.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't a number or `undefined'.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if gauge with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
set(Registry, Name, LabelValues, Value) ->
  Update =
    case Value of
      _ when is_number(Value) ->
        [{?IGAUGE_POS, 0}, {?FGAUGE_POS, Value}];
      undefined ->
        [{?IGAUGE_POS, undefined}, {?FGAUGE_POS, undefined}];
      _ -> erlang:error({invalid_value, Value, "set accepts only numbers and 'undefined'"})
    end,

  case ets:update_element(?TABLE, {Registry, Name, LabelValues},
                          Update) of
    false ->
      insert_metric(Registry, Name, LabelValues, Value, fun set/4);
    true ->
      ok
  end.

%% @equiv inc(default, Name, [], 1)
inc(Name) ->
  inc(default, Name, [], 1).

%% @doc If the second argument is a list, equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, LabelValues, 1)</tt></a>
%% otherwise equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, [], Value)</tt></a>.
inc(Name, LabelValues) when is_list(LabelValues)->
  inc(default, Name, LabelValues, 1);
inc(Name, Value) ->
  inc(default, Name, [], Value).

%% @equiv inc(default, Name, LabelValues, Value)
inc(Name, LabelValues, Value) ->
  inc(default, Name, LabelValues, Value).

%% @doc Increments the gauge identified by `Registry', `Name'
%% and `LabelValues' by `Value'.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't an integer.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if gauge with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
inc(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                       {?IGAUGE_POS, Value})
  catch error:badarg ->
      maybe_insert_metric_for_inc(Registry, Name, LabelValues, Value)
  end,
  ok;
inc(Registry, Name, LabelValues, Value) when is_number(Value) ->
  Key = key(Registry, Name, LabelValues),
  case ets:select_replace(?TABLE,
                          [{{Key, '$1', '$2'},
                            [],
                            [{{{Key}, '$1', {'+', '$2', Value}}}]}]) of
    0 ->
      insert_metric(Registry, Name, LabelValues, Value, fun inc/4);
    1 ->
      ok
  end;
inc(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "inc accepts only numbers"}).

%% @equiv inc(default, Name, [], -1)
dec(Name) ->
  inc(default, Name, [], -1).

%% @doc If the second argument is a list, equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, LabelValues, -1)</tt></a>
%% otherwise equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, [], -1 * Value)</tt></a>.
dec(Name, LabelValues) when is_list(LabelValues)->
  inc(default, Name, LabelValues, -1);
dec(Name, Value)  when is_number(Value) ->
  inc(default, Name, [], -1*Value);
dec(_Name, Value) ->
  erlang:error({invalid_value, Value,
                "dec accepts only numbers"}).

%% @equiv inc(default, Name, LabelValues, -1 * Value)
dec(Name, LabelValues, Value) when is_number(Value) ->
  inc(default, Name, LabelValues, -1*Value);
dec(_Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "dec accepts only numbers"}).


%% @equiv inc(Registry, Name, LabelValues, -1 * Value)
dec(Registry, Name, LabelValues, Value) when is_number(Value) ->
  inc(Registry, Name, LabelValues, -1*Value);
dec(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "dec accepts only numbers"}).

%% @equiv set_to_current_time(default, Name, [])
set_to_current_time(Name) ->
  set_to_current_time(default, Name, []).

%% @equiv set_to_current_time(default, Name, LabelValues)
set_to_current_time(Name, LabelValues) ->
  set_to_current_time(default, Name, LabelValues).

%% @doc Sets the gauge identified by `Registry', `Name'
%% and `LabelValues' to the current unixtime.
%%
%% Raises `{unknown_metric, Registry, Name}' error if gauge with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
set_to_current_time(Registry, Name, LabelValues) ->
  set(Registry, Name, LabelValues, os:system_time(seconds)).

%% @equiv track_inprogress(default, Name, [], Fun)
track_inprogress(Name, Fun) ->
  track_inprogress(default, Name, [], Fun).

%% @equiv track_inprogress(default, Name, LabelValues, Fun)
track_inprogress(Name, LabelValues, Fun) ->
  track_inprogress(default, Name, LabelValues, Fun).

%% @doc Sets the gauge identified by `Registry', `Name'
%% and `LabelValues' to the number of currently executing `Fun's.
%%
%% Raises `{unknown_metric, Registry, Name}' error if gauge with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% Raises `{invalid_value, Value, Message}' if `Fun'
%% isn't a function.<br/>
%% @end
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

%% @doc Sets the gauge identified by `Registry', `Name'
%% and `LabelValues' to the the amount of time spent executing `Fun'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if gauge with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% Raises `{invalid_value, Value, Message}' if `Fun'
%% isn't a function.<br/>
%% @end
set_duration(Registry, Name, LabelValues, Fun) when is_function(Fun) ->
  Start = erlang:monotonic_time(),
  try
    Fun()
  after
    set(Registry, Name, LabelValues, erlang:monotonic_time() - Start)
  end;
set_duration(_Registry, _Name, _LabelValues, Fun) ->
  erlang:error({invalid_value, Fun, "set_duration accepts only functions"}).

%% @equiv remove(default, Name, [])
remove(Name) ->
  remove(default, Name, []).

%% @equiv remove(default, Name, LabelValues)
remove(Name, LabelValues) ->
  remove(default, Name, LabelValues).

%% @doc Removes gauge series identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if gauge with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
remove(Registry, Name, LabelValues) ->
  prometheus_metric:remove_labels(?TABLE, Registry, Name, LabelValues).

%% @equiv match_remove(default, Name, [], [])
match_remove(Name) ->
  match_remove(default, Name, [], []).

%% @equiv match_remove(default, Name, LMatchHead, LMatchCond)
match_remove(Name, LMatchHead, LMatchCond) ->
  match_remove(default, Name, LMatchHead, LMatchCond).

%% @doc Remove the value of the gauge matched by `Registry', `Name',
%% `LMatchHead' and `LMatchCond'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if boolean with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
match_remove(Registry, Name, LMatchHead, LMatchCond) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LMatchHead),
  ets:select_delete(?TABLE, delete_match_spec(Registry, Name, LMatchHead, LMatchCond)).

%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

%% @doc Resets the value of the gauge identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if gauge with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  ets:update_element(?TABLE, {Registry, Name, LabelValues}, [{?IGAUGE_POS, 0}, {?FGAUGE_POS, 0}]).

%% @equiv value(default, Name, [])
value(Name) ->
  value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
value(Name, LabelValues) ->
  value(default, Name, LabelValues).

%% @doc Returns the value of the gauge identified by `Registry', `Name'
%% and `LabelValues'. If there is no gauge for `LabelValues',
%% returns `undefined'.
%%
%% If duration unit set, value will be converted to the duration unit.
%% {@link prometheus_time. Read more here.}
%%
%% Raises `{unknown_metric, Registry, Name}' error if gauge named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
value(Registry, Name, LabelValues) ->
  MF =  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  DU = prometheus_metric:mf_duration_unit(MF),
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, IValue, FValue}] -> prometheus_time:maybe_convert_to_du(DU, sum(IValue, FValue));
    [] -> undefined
  end.

values(Registry, Name) ->
  case prometheus_metric:check_mf_exists(?TABLE, Registry, Name) of
    false -> [];
    MF ->
      Labels = prometheus_metric:mf_labels(MF),
      DU = prometheus_metric:mf_duration_unit(MF),
      [{lists:zip(Labels, LabelValues),
        prometheus_time:maybe_convert_to_du(DU, sum(IValue, FValue))} ||
        [LabelValues, IValue, FValue] <- load_all_values(Registry, Name)]
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
  [Callback(create_gauge(Name, Help, {CLabels, Labels, Registry, DU})) ||
    [Name, {Labels, Help}, CLabels, DU, _] <- prometheus_metric:metrics(?TABLE,
                                                                        Registry)],
  ok.

%% @private
collect_metrics(Name, {CLabels, Labels, Registry, DU}) ->
  [prometheus_model_helpers:gauge_metric(
     CLabels ++ lists:zip(Labels, LabelValues),
     prometheus_time:maybe_convert_to_du(DU, sum(IValue, FValue))) ||
    [LabelValues, IValue, FValue] <- load_all_values(Registry, Name)].

%%====================================================================
%% Private Parts
%%====================================================================

key(Registry, Name, LabelValues) ->
  {Registry, Name, LabelValues}.

maybe_insert_metric_for_inc(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, undefined, undefined}] ->
      erlang:error({invalid_operation, 'inc/dec', "Can't inc/dec undefined"});
    _ ->
      insert_metric(Registry, Name, LabelValues, Value, fun inc/4)
  end.

deregister_select(Registry, Name) ->
  delete_match_spec(Registry, Name, '_', []).

delete_match_spec(Registry, Name, LMatchHead, LMatchCond) ->
  [{{{Registry, Name, LMatchHead}, '_', '_'}, LMatchCond, [true]}].

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {{Registry, Name, LabelValues}, 0, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

load_all_values(Registry, Name) ->
  ets:match(?TABLE, {{Registry, Name, '$1'}, '$2', '$3'}).

sum(_IValue, undefined) ->
  undefined;
sum(IValue, FValue) ->
  IValue + FValue.

create_gauge(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).
