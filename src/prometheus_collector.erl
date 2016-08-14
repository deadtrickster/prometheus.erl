-module(prometheus_collector).

-export([register/1,
         register/2,
         deregister/1,
         deregister/2,
         collect_mf/3]).

-export_type([collector/0,
              callback/0]).

-compile({no_auto_import, [register/2]}).

-include("prometheus_model.hrl").

-type collector() :: atom().

%% FIXME: temporary HACK
-type callback() ::
        fun ((atom(), atom(), list(atom()), string() | binary()) -> ok)
      | fun((_, _) -> any()).

%% FIXME: `| ok' is a temporary HACK
-callback collect_mf(Callback :: callback(),
                     Registry :: prometheus_registry:registry()) -> list() | ok.

%% FIXME: temporary HACK
-callback collect_metrics(Name, Data) -> Metrics when
    Name    :: atom(),
    Data    :: any(),
    Metrics :: #'Metric'{} | list(#'Metric'{}).

-callback deregister_cleanup(Registry :: prometheus_registry:registry()) -> ok.

%% @equiv register(Collector, default)
register(Collector) ->
  register(Collector, default).

register(Collector, Registry) ->
  ok = prometheus_registry:register_collector(Registry, Collector).

%% @equiv deregister(Collector, default)
deregister(Collector) ->
  deregister(Collector, default).

-spec deregister(Collector :: collector(),
                 Registry :: prometheus_registry:registry()) -> ok.
deregister(Collector, Registry) ->
  prometheus_registry:deregister_collector(Registry, Collector).

-spec collect_mf(Collector, Callback, Registry) -> list() when
    Collector :: collector(),
    Callback  :: callback(),
    Registry  :: prometheus_registry:registry().
collect_mf(Collector, Callback, Registry) ->
  Collector:collect_mf(Callback, Registry).
