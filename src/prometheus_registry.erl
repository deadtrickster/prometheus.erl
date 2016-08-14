-module(prometheus_registry).
-export([collect/2,
         collectors/1,
         register_collector/2,
         deregister_collector/2,
         collector_registeredp/2,
         clear/0,
         clear/1]).

-include("prometheus.hrl").

-define(TABLE, ?PROMETHEUS_REGISTRY_TABLE).

-spec collect(Registry, Callback) -> ok when
    Registry :: atom(),
    Callback :: prometheus_collector:callback().
collect(Registry, Callback) ->
  [Callback(Registry, Collector) ||
    {_, Collector} <- ets:lookup(?TABLE, Registry)],
  ok.

-spec collectors(Registry :: atom()) -> [Collector :: atom()].
collectors(Registry) ->
  [Collector || {_, Collector} <- ets:lookup(?TABLE, Registry)].

-spec register_collector(Registry :: atom(), Collector :: atom()) -> ok.
register_collector(Registry, Collector) ->
  ets:insert(?TABLE, {Registry, Collector}),
  ok.

deregister_collector(Registry, Collector) ->
  ets:delete_object(?TABLE, {Registry, Collector}),
  Collector:deregister_cleanup(Registry),
  ok.

%% @equiv clear(default)
-spec clear() -> ok.
clear() ->
  clear(default).

-spec clear(Registry :: atom()) -> ok.
clear(Registry) ->
  [Collector:deregister_cleanup(Registry) ||
    {_, Collector} <- ets:take(?TABLE, Registry)],
  ok.

-spec collector_registeredp(Registry, Collector) -> boolean() when
    Registry  :: atom(),
    Collector :: atom().
collector_registeredp(Registry, Collector) ->
  case ets:match(?TABLE, {Registry, Collector}) of
    [] -> false;
    _  -> true
  end.
