-module(prometheus_collector).

-export([register/1,
         register/2,
         deregister/1,
         deregister/2]).

-export_type([callback/0]).

-compile({no_auto_import, [register/2]}).

-include("prometheus_model.hrl").

%% FIXME: temporary HACK
-type callback() ::
        fun ((atom(), atom(), list(atom()), string() | binary()) -> ok)
      | fun((_, _) -> any()).

%% FIXME: `| ok' is a temporary HACK
-callback collect_mf(Callback :: callback(), Registry :: atom()) -> list() | ok.

%% FIXME: temporary HACK
-callback collect_metrics(Name :: atom(), Data :: any()) -> #'Metric'{} | list(#'Metric'{}).

-callback deregister_cleanup(Registry :: atom()) -> ok.

%% @equiv register(Collector, default)
register(Collector) ->
  register(Collector, default).

register(Collector, Registry) ->
  ok = prometheus_registry:register_collector(Registry, Collector).

%% @equiv deregister(Collector, default)
deregister(Collector) ->
  deregister(Collector, default).

-spec deregister(Collector :: atom(), Registry :: atom()) -> true.
deregister(Collector, Registry) ->
  prometheus_registry:deregister_collector(Registry, Collector).
