%%%-------------------------------------------------------------------
%% @doc prometheus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(prometheus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("prometheus.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  create_tables(),
  register_collectors(),
  register_metrics(),
  {ok, {{one_for_one, 5, 1}, [{prometheus_counter,
                               {prometheus_counter, start_link, []},
                               permanent,
                               5000,
                               worker,
                               [prometheus_counter]},
                              {prometheus_summary,
                               {prometheus_summary, start_link, []},
                               permanent,
                               5000,
                               worker,
                               [prometheus_summary]},
                              {prometheus_histogram,
                               {prometheus_histogram, start_link, []},
                               permanent,
                               5000,
                               worker,
                               [prometheus_histogram]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_tables() ->
  Tables = [
            {?PROMETHEUS_REGISTRY_TABLE, {bag, read_concurrency}},
            {?PROMETHEUS_COUNTER_TABLE, write_concurrency},
            {?PROMETHEUS_GAUGE_TABLE, write_concurrency},
            {?PROMETHEUS_SUMMARY_TABLE, write_concurrency},
            {?PROMETHEUS_HISTOGRAM_TABLE, write_concurrency}
           ],
  [maybe_create_table(Name, Concurrency) || {Name, Concurrency} <- Tables],
  ok.

register_collectors() ->
  [prometheus_collector:register(Collector) ||
    Collector <- enabled_collectors()].

register_metrics() ->
  [Metric:declare(Spec, Registry) ||
    {Registry, Metric, Spec} <- default_metrics()].

enabled_collectors() ->
  case application:get_env(prometheus, default_collectors) of
    undefined -> all_known_collectors();
    Collectors -> Collectors
  end.

all_known_collectors() ->
  prometheus_misc:behaviour_modules(prometheus_collector).

default_metrics() ->
  application:get_env(prometheus, default_metrics, []).

maybe_create_table(Name, {Type, Concurrency}) ->
  case ets:info(Name) of
    undefined ->
      ets:new(Name, [Type, named_table, public, {Concurrency, true}]);
    _ ->
      ok
  end;
maybe_create_table(Name, Concurrency) ->
  maybe_create_table(Name, {set, Concurrency}).
