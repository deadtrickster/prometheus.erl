-module(prometheus_collector).

-export([register/1,
         register/2,
         deregister/1,
         deregister/2]).

-compile({no_auto_import,[register/2]}).

-callback deregister_cleanup(Registry :: atom()) -> ok.

-callback collect_mf(Callback :: fun ((atom, atom, list(atom), string | binary) -> ok), Registry :: atom) -> ok.

-callback collect_metrics(Name :: atom, Data :: any()) -> ok.

register(Collector) ->
  register(Collector, default).

register(Collector, Registry) ->
  ok = prometheus_registry:register_collector(Registry, Collector).

deregister(Collector) ->
  deregister(Collector, default).

deregister(Collector, Registry) ->
  prometheus_registry:deregister_collector(Registry, Collector).
