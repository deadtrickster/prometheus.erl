%% @doc
%% Boolean metric, to report booleans and flags.
%%
%% Boolean is a non-standard metric that uses untyped metric underneath.
%%
%% A Boolean is typically used as a flag i.e. enabled/disabled, online/offline.
%%
%% Example:
%% <pre lang="erlang">
%% -module(my_fuse_instrumenter).
%%
%% -export([setup/0,
%%          fuse_event/2]).
%%
%% setup() ->
%%   prometheus_boolean:declare([{name, app_fuse_state},
%%                               {labels, [name]}, %% fuse name
%%                               {help, "State of various app fuses."}]),
%%
%% fuse_event(Fuse, Event) ->
%%   case Event of
%%     ok -> prometheus_boolean:set(app_fuse_state, [Fuse], true);
%%     blown -> prometheus_boolean:set(app_fuse_state, [Fuse], false);
%%     _ -> ok
%%   end.
%% </pre>
%% @end
-module(prometheus_boolean).

%%% metric
-export([new/1,
         declare/1,
         deregister/1,
         deregister/2,
         set_default/2,
         set/2,
         set/3,
         set/4,
         toggle/1,
         toggle/2,
         toggle/3,
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

-define(TABLE, ?PROMETHEUS_BOOLEAN_TABLE).
-define(BOOLEAN_POS, 2).

%%====================================================================
%% Metric API
%%====================================================================

%% @doc Creates a boolean using `Spec'.
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
%% Raises `{mf_already_exists, {Registry, Name}, Message}' error if a boolean
%% with the same `Spec' already exists.
%% @end
new(Spec) ->
  prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

%% @doc Creates a boolean using `Spec'.
%% If a boolean with the same `Spec' exists returns `false'.
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
%% @end
declare(Spec) ->
  prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec).

%% @equiv deregister(default, Name)
deregister(Name) ->
  deregister(default, Name).

%% @doc
%% Removes all boolean series with name `Name' and
%% removes Metric Family from `Registry'.
%%
%% After this call new/1 for `Name' and `Registry' will succeed.
%%
%% Returns `{true, _}' if `Name' was a registered boolean.
%% Otherwise returns `{true, _}'.
%% @end
deregister(Registry, Name) ->
  MFR = prometheus_metric:deregister_mf(?TABLE, Registry, Name),
  NumDeleted = ets:select_delete(?TABLE, deregister_select(Registry, Name)),
  {MFR, NumDeleted > 0}.

%% @private
set_default(Registry, Name) ->
  set(Registry, Name, [], undefined).

%% @equiv set(default, Name, [], Value)
set(Name, Value) ->
  set(default, Name, [], Value).

%% @equiv set(default, Name, LabelValues, Value)
set(Name, LabelValues, Value) ->
  set(default, Name, LabelValues, Value).

%% @doc Sets the boolean identified by `Registry', `Name'
%% and `LabelValues' to `Value'.
%%
%% Valid "truthy" values:
%% - `true`;
%% - `false`;
%% - `0` -> false;
%% - `number > 0` -> true;
%% - `[]` -> false
%% - `non-empty list` -> true;
%% - `undefined` -> undefined
%%
%% Other values will generate `invalid_value` error.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't a boolean or `undefined'.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if boolean with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
set(Registry, Name, LabelValues, Value0) ->
  Value = prometheus_model_helpers:boolean_value(Value0),
  set_(Registry, Name, LabelValues, Value).

%% @equiv toggle(default, Name, [])
toggle(Name) ->
  toggle(default, Name, []).

%% @equiv toggle(default, Name, LabelValues, Value)
toggle(Name, LabelValues) ->
  toggle(default, Name, LabelValues).

%% @doc Toggles the boolean identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% If boolean set to undefined, it can't be toggled.
%%
%% Raises `{invalid_value, undefined, Message}' if boolean is undefined.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if boolean with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
toggle(Registry, Name, LabelValues) ->
  try
    ets:update_counter(?TABLE, {Registry, Name, LabelValues},
                       {?BOOLEAN_POS, 1, 1, 0})
  catch error:badarg ->
      save_toggle(Registry, Name, LabelValues)
  end,
  ok.

%% @equiv remove(default, Name, [])
remove(Name) ->
  remove(default, Name, []).

%% @equiv remove(default, Name, LabelValues)
remove(Name, LabelValues) ->
  remove(default, Name, LabelValues).

%% @doc Removes boolean series identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if boolean with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
remove(Registry, Name, LabelValues) ->
  prometheus_metric:remove_labels(?TABLE, Registry, Name, LabelValues).

%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

%% @doc Resets the value of the boolean identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if boolean with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  ets:update_element(?TABLE, {Registry, Name, LabelValues}, {?BOOLEAN_POS, 0}).

%% @equiv value(default, Name, [])
value(Name) ->
  value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
value(Name, LabelValues) ->
  value(default, Name, LabelValues).

%% @doc Returns the value of the boolean identified by `Registry', `Name'
%% and `LabelValues'. If there is no boolean for `LabelValues',
%% returns `undefined'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if boolean named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
value(Registry, Name, LabelValues) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, 0}] ->
      false;
    [{_Key, 1}] ->
      true;
    [{_Key, undefined}] ->
      undefined;
    [] ->
      prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
      undefined
  end.

values(Registry, Name) ->
  case prometheus_metric:check_mf_exists(?TABLE, Registry, Name) of
    false -> [];
    MF ->
      Labels = prometheus_metric:mf_labels(MF),
      Arity = length(Labels),
      [{lists:zip(Labels, LabelValues), Value =:= 1} ||
        [LabelValues, Value] <- load_all_values(Registry, Name),
        Arity == length(LabelValues)]
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
collect_mf(Registry, Callback) ->
  [Callback(create_boolean(Name, Help, {CLabels, Labels, Registry})) ||
    [Name, {Labels, Help}, CLabels, _, _] <- prometheus_metric:metrics(?TABLE,
                                                                       Registry)],
  ok.

%% @private
collect_metrics(Name, {CLabels, Labels, Registry}) ->
  Arity = length(Labels),
  [prometheus_model_helpers:boolean_metric(
     CLabels ++ lists:zip(Labels, LabelValues), Value) ||
    [LabelValues, Value] <- load_all_values(Registry, Name),
    Arity == length(LabelValues)].

%%====================================================================
%% Private Parts
%%====================================================================

deregister_select(Registry, Name) ->
  [{{{Registry, Name, '_'}, '_'}, [], [true]}].

set_(Registry, Name, LabelValues, Value) ->
  case ets:update_element(?TABLE, {Registry, Name, LabelValues},
                          {?BOOLEAN_POS, Value}) of
    false ->
      insert_metric(Registry, Name, LabelValues, Value, fun set_/4);
    true ->
      ok
  end,
  ok.

save_toggle(Registry, Name, LabelValues) ->
  case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
    [{_Key, undefined}] ->
      erlang:error({invalid_value, undefined,
                    "can't toggle undefined boolean"});
    [] ->
      insert_metric(Registry, Name, LabelValues, 0, fun toggle/3)
  end.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {{Registry, Name, LabelValues}, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

load_all_values(Registry, Name) ->
  ets:match(?TABLE, {{Registry, Name, '$1'}, '$2'}).

create_boolean(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, boolean, ?MODULE, Data).
