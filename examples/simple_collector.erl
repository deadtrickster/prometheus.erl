%% @doc
%%
%% 1> c('examples/simple_collector').
%% {ok,simple_collector}
%% 2> prometheus_registry:register_collector(qwe, simple_collector).
%% ok
%% 3> io:format("~s", [prometheus_text_format:format(qwe)]).
%% # TYPE pool_size untyped
%% # HELP pool_size MongoDB Connections pool size
%% pool_size 365
%%
%% ok
%% @end

-module(simple_collector).

-behaviour(prometheus_collector).

-export([deregister_cleanup/1,
         collect_mf/2]).

-import(prometheus_model_helpers, [create_mf/4]).

%% ===================================================================
%% API
%% ===================================================================

%% called to collect Metric Families
collect_mf(_Registry, Callback) ->
  PoolSize = 365,
  Callback(create_mf(pool_size,
                     "MongoDB Connections pool size", untyped, PoolSize)),

  ok.

%% called when collector deregistered
deregister_cleanup(_Registry) -> ok.

%% ===================================================================
%% Private functions
%% ===================================================================
