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
         collect_mf/2,
         collect_metrics/2]).

%% ===================================================================
%% API
%% ===================================================================

%% called to collect Metric Families
collect_mf(_Registry, Callback) ->
  Data = #{pool_size => 365},
  Callback(create_untyped(pool_size,
                          "MongoDB Connections pool size", Data)),

  ok.

%% called to callect Time Series for the Metric Family
collect_metrics(pool_size, #{pool_size := PoolSize} = _Data) ->
  prometheus_model_helpers:untyped_metric(PoolSize).

%% called when collector deregistered
deregister_cleanup(_Registry) -> ok.

%% ===================================================================
%% Private functions
%% ===================================================================

create_untyped(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, untyped, ?MODULE, Data).
