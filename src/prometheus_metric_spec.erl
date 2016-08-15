-module(prometheus_metric_spec).

-export([get_value/2,
         get_value/3,
         fetch_value/2]).

-export_type([spec/0]).

%%====================================================================
%% Types
%%====================================================================

-type spec() :: proplists:proplist().

%%====================================================================
%% Public API
%%====================================================================

-spec get_value(Key :: atom(), Spec :: spec()) -> any().
get_value(Key, Spec) ->
  get_value(Key, Spec, undefined).

-spec get_value(Key :: atom(), Spec :: spec(), Default :: any()) -> any().
get_value(Key, Spec, Default) ->
  proplists:get_value(Key, Spec, Default).

-spec fetch_value(Key :: atom(), Spec :: spec()) -> any() | no_return().
fetch_value(Key, Spec) ->
  case proplists:get_value(Key, Spec) of
    undefined ->
      erlang:error({missing_metric_spec_key, Key, Spec});
    Value ->
      Value
  end.
