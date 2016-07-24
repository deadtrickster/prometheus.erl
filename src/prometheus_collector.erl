-module(prometheus_collector).

-export([deregister/2]).

-callback register(Registry :: atom) -> ok.

-callback register() -> ok.

-callback deregister(Registry :: atom) -> ok.

-callback collect_mf(Callback :: fun ((atom, atom, list(atom), string | binary) -> ok), Registry :: atom) -> ok.

-callback collect_metrics(Name :: atom, Data :: any()) -> ok.

deregister(Collector, Registry) ->
  Collector:deregister(Registry).
