-module(benchmark_counter).

-export([run/0,
         run_multi/2,
         run_semimulti/2
        ]).
-compile(inline).

-define(COUNT, 3 * 1000*1000).
-define(TIMEOUT, 100*1000).
-define(MILLION, 1000*1000).
-define(MILLIONS(C), C * ?MILLION).

setup() ->
  prometheus:start(),
  prometheus_counter:declare([{name, test_counter},
                              {help, ""}]).

run() ->
  io:format("Setting up environment~n"),
  setup(),
  io:format("Finished~n"),
  run_single_counter().

run_single_counter() ->
  
  prometheus_counter:reset(test_counter),
  
  run_test(1, ?MILLIONS(1), ?MILLIONS(1)),
  run_test(5, ?MILLIONS(1), ?MILLIONS(6)),
  run_test(10, ?MILLIONS(1), ?MILLIONS(16)),
  run_test(100, ?MILLIONS(1), ?MILLIONS(116)),
  
  io:format("Finished~n").

run_test(Pids, Count, ExpectedValue) ->
  io:format("Running single counter benchmark:~n"
            "  Pids: ~p~n"
            "  Iterations: ~p~n"
            "  Expected Value: ~p~n", [Pids, Count, ExpectedValue]),
  {Time, _} = timer:tc(fun() -> run_int(Pids, Count) end),
  io:format("  Duration (sec): ~p~n", [Time / 1000000]),
  Value = case prometheus_counter:value(test_counter) of
            ExpectedValue -> color:greenb(io_lib:format("~p", [ExpectedValue]));
            CValue -> color:redb(io_lib:format("~p", [CValue]))
          end,
  io:format("  Counter value: ~s~n", [Value]),
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
  trunc((X - (X rem Y)) / Y).
