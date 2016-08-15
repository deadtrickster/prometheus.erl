-module(prometheus_collector).

-export([register/1,
         register/2,
         deregister/1,
         deregister/2,
         collect_mf/3]).

-export_type([collector/0,
              datum/0,
              data/0,
              callback/0]).

-compile({no_auto_import, [register/2]}).

-type collector() :: atom().

-type datum() :: {atom(), non_neg_integer()}
               | {[{atom(), atom() | non_neg_integer()}, ...], atom()}
               | {_, _, [_]}.

-type data() :: datum() | [datum(), ...].

%% TODO: Split up callback types.
-type callback() ::
        fun ((atom(), atom(), list(atom()), string() | binary()) -> ok)
      | fun((_, _) -> any()).

-callback collect_mf(Callback, Registry) -> Metrics | ok when
    Callback :: callback(),
    Registry :: prometheus_registry:registry(),
    Metrics  :: [prometheus_model:'Metric'()].

-callback collect_metrics(Name, Data) -> Metrics when
    Name    :: atom(),
    Data    :: data(),
    Metrics :: prometheus_model:'Metric'() | [prometheus_model:'Metric'()].

-callback deregister_cleanup(Registry :: prometheus_registry:registry()) -> ok.

%% @equiv register(Collector, default)
register(Collector) -> register(Collector, default).

-spec register(Collector, Registry) -> ok when
    Collector :: collector(),
    Registry  :: prometheus_registry:registry().
register(Collector, Registry) ->
  ok = prometheus_registry:register_collector(Registry, Collector).

%% @equiv deregister(Collector, default)
deregister(Collector) -> deregister(Collector, default).

-spec deregister(Collector, Registry) -> ok when
    Collector :: collector(),
    Registry  :: prometheus_registry:registry().
deregister(Collector, Registry) ->
  prometheus_registry:deregister_collector(Registry, Collector).

-spec collect_mf(Collector, Callback, Registry) -> list() when
    Collector :: collector(),
    Callback  :: callback(),
    Registry  :: prometheus_registry:registry().
collect_mf(Collector, Callback, Registry) ->
  Collector:collect_mf(Callback, Registry).
