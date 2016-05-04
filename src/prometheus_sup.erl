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
  {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

create_tables() ->
  Tables = [
            {?PROMETHEUS_TABLE, [set, named_table, public, {read_concurrency, true}]},
            {?PROMETHEUS_COUNTER_TABLE, [set, named_table, public, {write_concurrency, true}]},
            {?PROMETHEUS_GAUGE_TABLE, [set, named_table, public, {write_concurrency, true}]},
            {?PROMETHEUS_SUMMARY_TABLE, [set, named_table, public, {write_concurrency, true}]}
           ],
  [maybe_create_table(ets:info(Name), Name, Opts) || {Name, Opts} <- Tables],
  [Collector:register() || Collector <- application:get_env(prometheus, default_collectors, ?PROMETHEUS_DEFAULT_COLLECTORS)],
  [Metric:new(Spec, Registry) || {Registry, Metric, Spec} <- application:get_env(prometheus, default_metrics, [])],
  ok.

maybe_create_table(undefined, Name, Opts) ->
  ets:new(Name, Opts);
maybe_create_table(_, _, _) ->
  ok.
