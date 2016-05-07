-module(prometheus_metric).
-export([insert_mf/5,
         insert_mf/6,
         check_mf_exists/4,
         mf_data/1,
         metrics/2,
         metrics_with_data/2]).

-include("prometheus.hrl").


-callback new(Info :: list()) -> ok.
-callback new(Info :: list(), Registry :: atom) -> ok.

-callback reset(Name :: atom) -> ok.
-callback reset(Name :: atom, LValues :: list()) -> ok.
-callback reset(Registry :: atom, Name :: atom, LValues :: list()) -> ok.

-callback value(Name :: atom) -> ok.
-callback value(Name :: atom, LValues :: list()) -> ok.
-callback value(Registry :: atom, Name :: atom, LValues :: list()) -> ok.

insert_mf(Table, Registry, Name, Labels, Help) ->
  insert_mf(Table, Registry, Name, Labels, Help, undefined).

insert_mf(Table, Registry, Name, Labels, Help, Data) ->
  true = ets:insert_new(Table, {{Registry, mf, Name}, Labels, Help, Data}).

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
          erlang:error({invalid_metric_arity}, LabelsLength, LabelValues)
      end
  end.

mf_data(MF) ->
  element(4, MF).

metrics(Table, Registry) ->
  ets:match(Table, {{Registry, mf, '$1'}, '$2', '$3'}).

metrics_with_data(Table, Registry) ->  
  ets:match(Table, {{Registry, mf, '$1'}, '$2', '$3', '$4'}).
