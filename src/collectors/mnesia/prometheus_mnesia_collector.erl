-module(prometheus_mnesia_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   counter_metric/1,
                                   gauge_metric/1]).

-behaviour(prometheus_collector).

%%====================================================================
%% Collector API
%%====================================================================
deregister_cleanup(_) -> ok.

-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
collect_mf(_Registry, Callback) ->
  case is_started(mnesia) of
    true -> collect_mf(Callback);
    false -> ok
  end,
  ok.

collect_mf(CB) ->
  SetMetrics = application:get_env(prometheus,mnesia_collector_metrics, all),

  {Participants, Coordinators} =
    case get_tm_info(SetMetrics) andalso mnesia_tm:get_info(1000) of
      {info, Ps, Cs} -> {length(Ps), length(Cs)};
      _ -> {undefined, undefined}    % gets transported as "NaN"
    end,

  Metrics =
    [{erlang_mnesia_held_locks,
      held_locks,
      "Number of held locks",
      gauge,
      fun() -> ets:info(mnesia_held_locks, size) end},
     {erlang_mnesia_lock_queue,
      lock_queue,
      "Number of transactions waiting for a lock",
      gauge,
      fun() -> ets:info(mnesia_lock_queue,size) end},
     {erlang_mnesia_transaction_participants,
      transaction_participants,
      "Number of participant transactions",
      gauge,
      fun() -> Participants end},
     {erlang_mnesia_transaction_coordinators,
      transaction_coordinators,
      "Number of coordinator transactions",
      gauge,
      fun() -> Coordinators end},
     {erlang_mnesia_failed_transactions,
      transaction_failures,
      "Number of failed (i.e. aborted) transactions",
      counter,
      fun() -> mnesia:system_info(transaction_failures) end},
     {erlang_mnesia_committed_transactions,
      transaction_commits,
      "Number of commited transactions",
      counter,
      fun() -> mnesia:system_info(transaction_commits) end},
     {erlang_mnesia_logged_transactions,
      transaction_log_writes,
      "Number of transactions logged",
      counter,
      fun() -> mnesia:system_info(transaction_log_writes) end},
     {erlang_mnesia_restarted_transactions,
      transaction_restarts,
      "Total number of transaction restarts",
      counter,
      fun() -> mnesia:system_info(transaction_restarts) end}],

  lists:foreach(fun(Metric) -> emit(CB, Metric, SetMetrics) end, Metrics).

get_tm_info(SetMetrics) ->
  (SetMetrics =:= all)
    orelse
      (lists:member(transaction_participants, SetMetrics)
       orelse
       lists:member(transaction_coordinators, SetMetrics)).

emit(CB, {MetricName, Selector, Help, Type, DF}, SetMetrics) ->
  case SetMetrics =:= all orelse lists:member(Selector, SetMetrics) of
    false -> ok;
    true -> CB(create_mf(MetricName, Help, Type, ?MODULE, {Type, datafun(DF)}))
  end.

datafun(DataFun) ->
  try DataFun()
  catch _:_ -> undefined
  end.

collect_metrics(_Key, {counter, Val}) -> counter_metric(Val);
collect_metrics(_Key, {gauge, Val}) -> gauge_metric(Val).

is_started(App) ->
  case [V || {A,_,V} <- application:which_applications(), A == App] of
    [] -> false;
    [_] -> true
  end.
