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

collect_mf(Callback) ->
  {Participants, Coordinators} =
    case mnesia_tm:get_info(1000) of
      {info, Ps, Cs} -> {length(Ps), length(Cs)};
      _ -> {undefined, undefined}    % gets transported as "NaN"
    end,

  Callback(create_mf(erlang_mnesia_held_locks,
                     "Number of held locks",
                     gauge, ?MODULE,
                     ets:info(mnesia_held_locks, size))),
  Callback(create_mf(erlang_mnesia_lock_queue,
                     "Number of transactions waiting for a lock",
                     gauge, ?MODULE,
                     ets:info(mnesia_lock_queue,size))),
  Callback(create_mf(erlang_mnesia_transaction_participants,
                     "Number of participant transactions",
                     gauge, ?MODULE,
                     Participants)),
  Callback(create_mf(erlang_mnesia_transaction_coordinators,
                     "Number of coordinator transactions",
                     gauge, ?MODULE,
                     Coordinators)),
  Callback(create_mf(erlang_mnesia_failed_transactions,
                     "Number of failed (i.e. aborted) transactions",
                     counter, ?MODULE,
                     mnesia:system_info(transaction_failures))),
  Callback(create_mf(erlang_mnesia_committed_transactions,
                     "Number of commited transactions",
                     counter, ?MODULE,
                     mnesia:system_info(transaction_commits))),
  Callback(create_mf(erlang_mnesia_logged_transactions,
                     "Number of transactions logged",
                     counter, ?MODULE,
                     mnesia:system_info(transaction_log_writes))),
  Callback(create_mf(erlang_mnesia_restarted_transactions,
                     "Total number of transaction restarts",
                     counter, ?MODULE,
                     mnesia:system_info(transaction_restarts))).

-define(COUNTERS,
        [erlang_mnesia_failed_transactions,
         erlang_mnesia_committed_transactions,
         erlang_mnesia_logged_transactions,
         erlang_mnesia_restarted_transactions]).
-define(GAUGES,
        [erlang_mnesia_held_locks,
         erlang_mnesia_lock_queue,
         erlang_mnesia_transaction_participants,
         erlang_mnesia_transaction_coordinators,
         erlang_mnesia_current_transactions]).

collect_metrics(Key,Val) ->
  case {lists:member(Key, ?COUNTERS), lists:member(Key, ?GAUGES)} of
    {true, false} -> counter_metric(Val);
    {false, true} -> gauge_metric(Val)
  end.

is_started(App) ->
  case [V || {A,_,V} <- application:which_applications(), A == App] of
    [] -> false;
    [_] -> true
  end.
