%% @doc
%% A collector for a set of metrics.
%%
%% Normal users should use {@link prometheus_gauge},
%% {@link prometheus_counter}, {@link prometheus_summary}
%% and {@link prometheus_histogram}.
%%
%% Implementing `:prometheus_collector' behaviour is for advanced uses,
%% such as proxying metrics from another monitoring system.
%% It is it the responsibility of the implementer to ensure produced metrics
%% are valid.
%%
%% You will be working with Prometheus
%% data model directly (see {@link prometheus_model_helpers}).
%% @end
-module(prometheus_collector).

-export([enabled_collectors/0,
         register/1,
         register/2,
         deregister/1,
         deregister/2,
         collect_mf/3]).

-export_type([collector/0,
              data/0,
              collect_mf_callback/0]).

-compile({no_auto_import, [register/2]}).

-include("prometheus.hrl").

%%====================================================================
%% Types
%%====================================================================

-type collector() :: atom().

-type data() :: any().

-type collect_mf_callback() ::
        fun((prometheus_model:'MetricFamily'()) -> any()).

%%====================================================================
%% Callbacks
%%====================================================================

-callback collect_mf(Callback, Registry) -> Metrics | ok when
    Callback :: collect_mf_callback(),
    Registry :: prometheus_registry:registry(),
    Metrics  :: [prometheus_model:'Metric'()].

-callback collect_metrics(Name, Data) -> Metrics when
    Name    :: prometheus_metric:name(),
    Data    :: data(),
    Metrics :: prometheus_model:'Metric'() | [prometheus_model:'Metric'()].

-callback deregister_cleanup(Registry) -> ok when
    Registry :: prometheus_registry:registry().

%%====================================================================
%% Public API
%%====================================================================

-spec enabled_collectors() -> [collector()].
enabled_collectors() ->
  case application:get_env(prometheus, default_collectors) of
    undefined -> all_known_collectors();
    Collectors -> Collectors
  end.

%% @equiv register(Collector, default)
%% @deprecated Please use {@link prometheus_registry:register_collector/1}
register(Collector) -> register(Collector, default).

-spec register(Collector, Registry) -> ok when
    Collector :: collector(),
    Registry  :: prometheus_registry:registry().
%% @deprecated Please use {@link prometheus_registry:register_collector/2}
register(Collector, Registry) ->
  ?DEPRECATED("prometheus_collector:register/2",
              "prometheus_register:register_collector/2"),
  ok = prometheus_registry:register_collector(Registry, Collector).

%% @equiv deregister(Collector, default)
%% @deprecated Please use {@link prometheus_registry:deregister_collector/1}
deregister(Collector) -> deregister(Collector, default).

-spec deregister(Collector, Registry) -> ok when
    Collector :: collector(),
    Registry  :: prometheus_registry:registry().
%% @deprecated Please use {@link prometheus_registry:deregister_collector/2}
deregister(Collector, Registry) ->
  ?DEPRECATED("prometheus_collector:deregister/2",
              "prometheus_register:deregister_collector/2"),
  prometheus_registry:deregister_collector(Registry, Collector).

-spec collect_mf(Collector, Callback, Registry) -> list() when
    Collector :: collector(),
    Callback  :: collect_mf_callback(),
    Registry  :: prometheus_registry:registry().
collect_mf(Collector, Callback, Registry) ->
  Collector:collect_mf(Callback, Registry).

%%====================================================================
%% Private Parts
%%====================================================================

all_known_collectors() ->
  prometheus_misc:behaviour_modules(prometheus_collector).
