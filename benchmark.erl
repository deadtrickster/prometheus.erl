-module(benchmark).

-export([run/2,
         run_multi/2,
         run_semimulti/2
        ]).
-compile(inline).

-define(COUNT, 3 * 1000*1000).
-define(TIMEOUT, 3000*1000).

run(Pids, Count) ->
  prometheus:start(),
  prometheus_counter:declare([{name, test_counter},
                              {help, ""}]),
  prometheus_counter:reset(test_counter),
  {Time, _} = timer:tc(fun() -> run_int(Pids, Count) end),
  {Time, prometheus_counter:value(test_counter)}.

run_int(PCount, Count) ->
  Mgr = self(),
  Pids = [spawn_link(fun() -> loop(Mgr, Count, test_counter) end) || _ <- lists:seq(1, PCount)],
  collect(Pids).

collect([]) -> ok;
collect([Pid | Pids]) ->
  receive
    {ok, Pid} -> collect(Pids)
  after ?TIMEOUT ->
      {error, timeout}
  end.

loop(Mgr, 0, _Name) ->
  Mgr ! {ok, self()};
loop(Mgr, N, Name) ->
  prometheus_counter:inc(Name, 1),
  loop(Mgr, N-1, Name).


multi_counter_name(N) ->
  list_to_binary(io_lib:format("counter_~p", [N])).

init_multi_counter(N) ->  
  prometheus_counter:declare([{name, multi_counter_name(N)},
                              {help, ""}]),  
  prometheus_counter:reset(multi_counter_name(N)).

run_multi(Pids, Count) ->
  prometheus:start(),
  [init_multi_counter(N) || N <- lists:seq(1, Pids)],
  prometheus_counter:declare([{name, test_counter},
                              {help, ""}]),
  prometheus_counter:reset(test_counter),
  {Time, _} = timer:tc(fun() -> run_int_multi(Pids, Count) end),
  {Time, [prometheus_counter:value(multi_counter_name(N)) || N <- lists:seq(1, Pids)]}.

run_int_multi(PCount, Count) ->
  Mgr = self(),
  Pids = [spawn_link(fun() -> loop(Mgr, Count, multi_counter_name(N)) end) || N <- lists:seq(1, PCount)],
  collect(Pids).

run_semimulti(Pids, Count) ->
  prometheus:start(),
  [init_multi_counter(floor(N, 2)) || N <- lists:seq(1, Pids)],
  prometheus_counter:declare([{name, test_counter},
                              {help, ""}]),
  prometheus_counter:reset(test_counter),
  {Time, _} = timer:tc(fun() -> run_int_semimulti(Pids, Count) end),
  {Time, [prometheus_counter:value(multi_counter_name(floor(N, 2))) || N <- lists:seq(1, Pids)]}.

run_int_semimulti(PCount, Count) ->
  Mgr = self(),
  Pids = [spawn_link(fun() -> loop(Mgr, Count, multi_counter_name(floor(N, 2))) end) || N <- lists:seq(1, PCount)],
  collect(Pids).

floor(X,Y) ->
+  trunc((X - (X rem Y)) / Y).

%% run(CoreCount) ->
%%   Mgr = self(),
%%   Pids = [spawn_link(fun() -> loop(Mgr, ?COUNT) end) || _ <- lists:seq(1, CoreCount)],
%%   collect(Pids).

%% collect([]) -> ok;
%% collect([Pid | Pids]) ->
%%     receive
%%         {ok, Pid} -> collect(Pids)
%%     after ?TIMEOUT ->
%%         {error, timeout}
%%     end.

%% loop(Mgr, 0) ->
%% 	Mgr ! {ok, self()};
%% loop(Mgr, N) ->
%% 	prometheus_counter:dinc(test_counter),
%% 	loop(Mgr, N-1).
