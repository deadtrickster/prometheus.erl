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
                               [prometheus_summary]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_tables() ->
  Tables = [
            {?PROMETHEUS_TABLE, [bag, named_table, public, {read_concurrency, true}]},
            {?PROMETHEUS_COUNTER_TABLE, [set, named_table, public, {write_concurrency, true}]},
            {?PROMETHEUS_GAUGE_TABLE, [set, named_table, public, {write_concurrency, true}]},
            {?PROMETHEUS_SUMMARY_TABLE, [set, named_table, public, {write_concurrency, true}]},
            {?PROMETHEUS_HISTOGRAM_TABLE, [set, named_table, public, {write_concurrency, true}]}
           ],
  [maybe_create_table(ets:info(Name), Name, Opts) || {Name, Opts} <- Tables],
  ok.

register_collectors() ->
  [Collector:register() || Collector <- enabled_collectors()].

register_metrics() ->
  [Metric:register(Spec, Registry) || {Registry, Metric, Spec} <- application:get_env(prometheus, default_metrics, [])].

enabled_collectors() ->
  case application:get_env(prometheus, default_collectors) of
    undefined -> all_known_collectors();
    Collectors -> Collectors
  end.

all_known_collectors() ->
  [Module || {_App, Module, Behaviours} <-
               prometheus_misc:all_module_attributes(behaviour),
             not lists:member(prometheus_metric, Behaviours),
             lists:member(prometheus_collector, Behaviours)].

maybe_create_table(undefined, Name, Opts) ->
  ets:new(Name, Opts);
maybe_create_table(_, _, _) ->
  ok.
