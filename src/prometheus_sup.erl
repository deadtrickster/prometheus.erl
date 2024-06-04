%%%-------------------------------------------------------------------
%% @doc prometheus top level supervisor.
%% @hidden
%%%-------------------------------------------------------------------

-module(prometheus_sup).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-behaviour(supervisor).

%%====================================================================
%% Macros
%%====================================================================

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
  ChildSpec =
    [{prometheus_startup_server,
      {prometheus_startup, start_link, []},
      permanent,
      1000,
      worker,
      [prometheus_startup]}],
  {ok, {{one_for_one, 5, 1}, ChildSpec}}.