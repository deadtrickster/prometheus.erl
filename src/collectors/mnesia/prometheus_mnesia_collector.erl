%% @doc
%% Collects Mnesia metrics mainly using
%% <a href="http://erlang.org/doc/man/mnesia.html#system_info-1">
%%   mnesia:system_info/1
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang_mnesia_held_locks'<br/>
%%     Type: gauge.<br/>
%%     Number of held locks.
%%   </li>
%%   <li>
%%     `erlang_mnesia_lock_queue'<br/>
%%     Type: gauge.<br/>
%%     Number of transactions waiting for a lock.
%%   </li>
%%   <li>
%%     `erlang_mnesia_transaction_participants'<br/>
%%     Type: gauge.<br/>
%%     Number of participant transactions.
%%   </li>
%%   <li>
%%     `erlang_mnesia_transaction_coordinators'<br/>
%%     Type: gauge.<br/>
%%     Number of coordinator transactions.
%%   </li>
%%   <li>
%%     `erlang_mnesia_failed_transactions'<br/>
%%     Type: counter.<br/>
%%     Number of failed (i.e. aborted) transactions.
%%   </li>
%%   <li>
%%     `erlang_mnesia_committed_transactions'<br/>
%%     Type: gauge.<br/>
%%     Number of committed transactions.
%%   </li>
%%   <li>
%%     `erlang_mnesia_logged_transactions'<br/>
%%     Type: counter.<br/>
%%     Number of transactions logged.
%%   </li>
%%   <li>
%%     `erlang_mnesia_restarted_transactions'<br/>
%%     Type: counter.<br/>
%%     Total number of transaction restarts.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `mnesia_collector_metrics' key of `prometheus' app environment.
%%
%% Available options:
%% - `held_locks' for `erlang_mnesia_held_locks';
%% - `lock_queue' for `erlang_mnesia_lock_queue';
%% - `transaction_participants' for `erlang_mnesia_transaction_participants';
%% - `transaction_coordinators' for `erlang_mnesia_transaction_coordinators';
%% - `transaction_failures' for `erlang_mnesia_failed_transactions';
%% - `transaction_commits' for `erlang_mnesia_committed_transactions';
%% - `transaction_log_writes' for `erlang_mnesia_logged_transactions';
%% - `transaction_restarts' for `erlang_mnesia_restarted_transactions'.
%%
%% By default all metrics are enabled.
%%
%% @end

-module(prometheus_mnesia_collector).

-export([deregister_cleanup/1,
         collect_mf/2]).

-import(prometheus_model_helpers, [create_mf/4]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(METRIC_NAME_PREFIX, "erlang_mnesia_").

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) -> ok.

%% @private
-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
collect_mf(_Registry, Callback) ->
  case is_started(mnesia) of
    true ->
      EnabledMetrics = enabled_metrics(),
      Metrics = metrics(EnabledMetrics),
      [add_metric_family(Metric, Callback)
       || {Name, _, _, _}=Metric <- Metrics, metric_enabled(Name, EnabledMetrics)];
    false -> ok
  end,
  ok.

add_metric_family({Name, Type, Help, Metrics}, Callback) ->
  Callback(create_mf(?METRIC_NAME(Name), Help, Type, catch_all(Metrics))).

%%====================================================================
%% Private Parts
%%====================================================================

metrics(EnabledMetrics) ->
  {Participants, Coordinators} = get_tm_info(EnabledMetrics),

  [{held_locks, gauge,
    "Number of held locks.",
    fun() -> ets:info(mnesia_held_locks, size) end},
   {lock_queue, gauge,
    "Number of transactions waiting for a lock.",
    fun() -> ets:info(mnesia_lock_queue, size) end},
   {transaction_participants, gauge,
    "Number of participant transactions.",
    fun() -> Participants end},
   {transaction_coordinators, gauge,
    "Number of coordinator transactions.",
    fun() -> Coordinators end},
   {failed_transactions, counter,
    "Number of failed (i.e. aborted) transactions.",
    fun() -> mnesia:system_info(transaction_failures) end},
   {committed_transactions, counter,
    "Number of committed transactions.",
    fun() -> mnesia:system_info(transaction_commits) end},
   {logged_transactions, counter,
    "Number of transactions logged.",
    fun() -> mnesia:system_info(transaction_log_writes) end},
   {restarted_transactions, counter,
    "Total number of transaction restarts.",
    fun() -> mnesia:system_info(transaction_restarts) end}].

%%====================================================================
%% Private Parts
%%====================================================================

get_tm_info(SetMetrics) ->
  case tm_metrics_enabled(SetMetrics) of
    true ->
      prometheus_mnesia:tm_info();
    _ ->
      {undefined, undefined}
  end.

tm_metrics_enabled(SetMetrics) ->
  metric_enabled(transaction_participants, SetMetrics) orelse
    metric_enabled(transaction_coordinators, SetMetrics).

catch_all(DataFun) ->
    try DataFun()
    catch _:_ -> undefined
    end.

is_started(App) ->
  case [V || {A,_,V} <- application:which_applications(), A == App] of
    [] -> false;
    [_] -> true
  end.

enabled_metrics() ->
  application:get_env(prometheus, mnesia_collector_metrics, all).

metric_enabled(Name, Metrics) ->
  Metrics =:= all orelse lists:member(Name, Metrics).
