-module(prometheus_metric).

-export([insert_new_mf/5,
         insert_new_mf/6,
         insert_mf/5,
         insert_mf/6,
         deregister_mf/2,
         check_mf_exists/4,
         mf_data/1,
         metrics/2,
         extract_common_params/1,
         extract_key_or_default/3,
         extract_key_or_raise_missing/2]).

-export_type([value/0]).

-ifdef(TEST).
-export([validate_metric_name/1,
         validate_metric_label_names/1,
         validate_metric_help/1]).
-endif.

-include("prometheus.hrl").


-callback new(Info :: list()) -> ok.
-callback new(Info :: list(), Registry :: atom()) -> ok.

-callback declare(Info :: list()) -> boolean().
-callback declare(Info :: list(), Registry :: atom()) -> boolean().

-callback reset(Name :: atom()) -> boolean().
-callback reset(Name :: atom(), LValues :: list()) -> boolean().
-callback reset(Registry :: atom(), Name :: atom(), LValues :: list()) -> boolean().

-type value() :: {Count :: number(), Sum :: number()}
                 %% FIXME: temporary HACK
               | {[any()], any()}.

-callback value(Name :: atom()) -> value().
-callback value(Name :: atom(), LValues :: list()) -> value().
-callback value(Registry :: atom(), Name :: atom(), LValues :: list()) -> value().

insert_new_mf(Table, Registry, Name, Labels, Help) ->
  insert_new_mf(Table, Registry, Name, Labels, Help, undefined).

insert_new_mf(Table, Registry, Name, Labels, Help, Data) ->
  case insert_mf(Table, Registry, Name, Labels, Help, Data) of
    true ->
      ok;
    false ->
      erlang:error({mf_already_exists, {Registry, Name}, "maybe you could try declare?"})
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
  RawName = extract_key_or_raise_missing(name, Spec),
  Name = validate_metric_name(RawName),

  RawLabels = extract_key_or_default(labels, Spec, []),
  Labels = validate_metric_label_names(RawLabels),

  RawHelp = extract_key_or_raise_missing(help, Spec),
  Help = validate_metric_help(RawHelp),

  {Name, Labels, Help}.

extract_key_or_raise_missing(Key, Spec) ->
  case proplists:get_value(Key, Spec) of
    undefined ->
      erlang:error({missing_metric_spec_key, Key, Spec});
    RawValue ->
      RawValue
  end.

extract_key_or_default(Key, Spec, Default) ->
  proplists:get_value(Key, Spec, Default).

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
      case re:run(ListName, "^[a-zA-Z_:][a-zA-Z0-9_:]*$") of
        {match, _} -> RawName;
                    nomatch -> erlang:error({invalid_metric_name, RawName, "metric name doesn't match regex [a-zA-Z_:][a-zA-Z0-9_:]*"})
                 end;
                    false ->
                     erlang:error({invalid_metric_name, RawName, "metric name is invalid string"})
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
      erlang:error({invalid_metric_label_name, RawName, "metric label is invalid string"})
  end;
validate_metric_label_name(RawName) ->
  erlang:error({invalid_metric_label_name, RawName, "metric label is not a string"}).

validate_metric_label_name_content("__"  ++ _Rest = RawName) ->
  erlang:error({invalid_metric_label_name, RawName, "metric label can't start with __"});
validate_metric_label_name_content(RawName) ->
  case re:run(RawName, "^[a-zA-Z_][a-zA-Z0-9_]*$") of
    {match, _} -> RawName;
    nomatch -> erlang:error({invalid_metric_label_name, RawName, "metric label doesn't match regex [a-zA-Z_][a-zA-Z0-9_]*"})
  end.

validate_metric_help(RawHelp) when is_binary(RawHelp) ->
  validate_metric_help(binary_to_list(RawHelp));
validate_metric_help(RawHelp) when is_list(RawHelp) ->
  case io_lib:printable_unicode_list(RawHelp) of
    true ->
      RawHelp;
    false ->
      erlang:error({invalid_metric_help, RawHelp, "metric help is invalid string"})
  end;
validate_metric_help(RawHelp) ->
  erlang:error({invalid_metric_help, RawHelp, "metric help is not a string"}).
