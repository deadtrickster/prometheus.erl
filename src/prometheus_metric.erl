-module(prometheus_metric).

-export([insert_new_mf/5,
         insert_new_mf/6,
         insert_mf/5,
         insert_mf/6,
         deregister_mf/2,
         check_mf_exists/4,
         mf_data/1,
         metrics/2,
         extract_common_params/1]).

-export_type([name/0,
              value/0]).

-ifdef(TEST).
-export([validate_metric_name/1,
         validate_metric_label_names/1,
         validate_metric_help/1]).
-endif.

-include("prometheus.hrl").

-type name() :: atom() | binary() | nonempty_string().

-type value() :: {Count :: number(), Sum :: number()}
                 %% FIXME: temporary HACK
               | {[any()], any()}.

%%====================================================================
%% Callbacks
%%====================================================================

-callback new(Info :: list()) -> ok.
-callback new(Info :: list(), Registry :: prometheus_registry:registry()) -> ok.

-callback declare(Info :: list()) -> boolean().
-callback declare(Info :: list(),
                  Registry :: prometheus_registry:registry()) -> boolean().

-callback reset(Name :: name()) -> boolean().
-callback reset(Name :: name(), LValues :: list()) -> boolean().
-callback reset(Registry, Name, LValues) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Name     :: name(),
    LValues  :: list().

-callback value(Name :: name()) -> value().
-callback value(Name :: name(), LValues :: list()) -> value().
-callback value(Registry, Name, LValues) -> value() when
    Registry :: prometheus_registry:registry(),
    Name     :: name(),
    LValues  :: list().

%%====================================================================
%% Public API
%%====================================================================

insert_new_mf(Table, Registry, Name, Labels, Help) ->
  insert_new_mf(Table, Registry, Name, Labels, Help, undefined).

insert_new_mf(Table, Registry, Name, Labels, Help, Data) ->
  case insert_mf(Table, Registry, Name, Labels, Help, Data) of
    true ->
      ok;
    false ->
      erlang:error({mf_already_exists, {Registry, Name},
                    "Consider using declare instead."})
  end.

insert_mf(Table, Registry, Name, Labels, Help) ->
  insert_mf(Table, Registry, Name, Labels, Help, undefined).

insert_mf(Table, Registry, Name, Labels, Help, Data) ->
  ets:insert_new(Table, {{Registry, mf, Name}, Labels, Help, Data}).

deregister_mf(Table, Registry) ->
  ets:match_delete(Table, {{Registry, mf, '_'}, '_', '_', '_'}).

check_mf_exists(Table, Registry, Name, LabelValues) ->
  case ets:lookup(Table, {Registry, mf, Name}) of
    [] ->
      erlang:error({unknown_metric, Registry, Name});
    [{_, Labels, _, _} = MF] ->
      LVLength = length(LabelValues),
      case length(Labels) of
        LVLength ->
          MF;
        LabelsLength ->
          erlang:error({invalid_metric_arity, LabelsLength}, LabelValues)
      end
  end.

mf_data(MF) ->
  element(4, MF).

metrics(Table, Registry) ->
  ets:match(Table, {{Registry, mf, '$1'}, '$2', '$3', '$4'}).

extract_common_params(Spec) ->
  RawName = prometheus_metric_spec:fetch_value(name, Spec),
  Name = validate_metric_name(RawName),

  RawLabels = prometheus_metric_spec:get_value(labels, Spec, []),
  Labels = validate_metric_label_names(RawLabels),

  RawHelp = prometheus_metric_spec:fetch_value(help, Spec),
  Help = validate_metric_help(RawHelp),

  {Name, Labels, Help}.

%%====================================================================
%% Private Parts
%%====================================================================

validate_metric_name(RawName) when is_atom(RawName) ->
  validate_metric_name(RawName, atom_to_list(RawName));
validate_metric_name(RawName) when is_binary(RawName) ->
  validate_metric_name(RawName, binary_to_list(RawName));
validate_metric_name(RawName) when is_list(RawName) ->
  validate_metric_name(RawName, RawName);
validate_metric_name(RawName) ->
  erlang:error({invalid_metric_name, RawName, "metric name is not a string"}).

validate_metric_name(RawName, ListName) ->
  case io_lib:printable_unicode_list(ListName) of
    true ->
      Regex = "^[a-zA-Z_:][a-zA-Z0-9_:]*$",
      case re:run(ListName, Regex) of
        {match, _} ->
          RawName;
        nomatch ->
          erlang:error({invalid_metric_name, RawName,
                        "metric name doesn't match regex " ++ Regex})
      end;
    false ->
      erlang:error({invalid_metric_name, RawName,
                    "metric name is invalid string"})
  end.

validate_metric_label_names(RawLabels) when is_list(RawLabels) ->
  lists:map(fun validate_metric_label_name/1, RawLabels);
validate_metric_label_names(RawLabels) ->
  erlang:error({invalid_metric_labels, RawLabels, "not list"}).

validate_metric_label_name(RawName) when is_atom(RawName) ->
  validate_metric_label_name(atom_to_list(RawName));
validate_metric_label_name(RawName) when is_binary(RawName) ->
  validate_metric_label_name(binary_to_list(RawName));
validate_metric_label_name(RawName) when is_list(RawName) ->
  case io_lib:printable_unicode_list(RawName) of
    true ->
      validate_metric_label_name_content(RawName);
    false ->
      erlang:error({invalid_metric_label_name, RawName,
                    "metric label is invalid string"})
  end;
validate_metric_label_name(RawName) ->
  erlang:error({invalid_metric_label_name, RawName,
                "metric label is not a string"}).

validate_metric_label_name_content("__"  ++ _Rest = RawName) ->
  erlang:error({invalid_metric_label_name, RawName,
                "metric label can't start with __"});
validate_metric_label_name_content(RawName) ->
  Regex = "^[a-zA-Z_][a-zA-Z0-9_]*$",
  case re:run(RawName, Regex) of
    {match, _} -> RawName;
    nomatch ->
      erlang:error({invalid_metric_label_name, RawName,
                    "metric label doesn't match regex " ++ Regex})
  end.

validate_metric_help(RawHelp) when is_binary(RawHelp) ->
  validate_metric_help(binary_to_list(RawHelp));
validate_metric_help(RawHelp) when is_list(RawHelp) ->
  case io_lib:printable_unicode_list(RawHelp) of
    true  -> RawHelp;
    false -> erlang:error({invalid_metric_help, RawHelp,
                           "metric help is invalid string"})
  end;
validate_metric_help(RawHelp) ->
  erlang:error({invalid_metric_help, RawHelp, "metric help is not a string"}).
