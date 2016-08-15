-module(prometheus_registry).

-export([collect/2,
         collectors/1,
         register_collector/2,
         deregister_collector/2,
         clear/0,
         clear/1,
         collector_registeredp/2]).

-export_type([registry/0]).

-include("prometheus.hrl").

%%====================================================================
%% Types
%%====================================================================

-type registry() :: atom().

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_REGISTRY_TABLE).

%%====================================================================
%% Public API
%%====================================================================

-spec collect(Registry, Callback) -> ok when
    Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
collect(Registry, Callback) ->
  [Callback(Registry, Collector) ||
    {_, Collector} <- ets:lookup(?TABLE, Registry)],
  ok.

-spec collectors(Registry :: prometheus_registry:registry())
                -> [Collector :: prometheus_collector:collector()].
collectors(Registry) ->
  [Collector || {_, Collector} <- ets:lookup(?TABLE, Registry)].

-spec register_collector(Registry :: prometheus_registry:registry(),
                         Collector :: prometheus_collector:collector()) -> ok.
register_collector(Registry, Collector) ->
  ets:insert(?TABLE, {Registry, Collector}),
  ok.

-spec deregister_collector(Registry :: prometheus_registry:registry(),
                           Collector :: prometheus_collector:collector()) -> ok.
deregister_collector(Registry, Collector) ->
  ets:delete_object(?TABLE, {Registry, Collector}),
  Collector:deregister_cleanup(Registry),
  ok.

%% @equiv clear(default)
-spec clear() -> ok.
clear() ->
  clear(default).

-spec clear(Registry :: prometheus_registry:registry()) -> ok.
clear(Registry) ->
  [Collector:deregister_cleanup(Registry) ||
    {_, Collector} <- ets:take(?TABLE, Registry)],
  ok.

-spec collector_registeredp(Registry, Collector) -> boolean() when
    Registry  :: prometheus_registry:registry(),
    Collector :: prometheus_collector:collector().
collector_registeredp(Registry, Collector) ->
  case ets:match(?TABLE, {Registry, Collector}) of
    [] -> false;
    _  -> true
  end.
