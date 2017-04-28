-module(prometheus_nif_counter).
-on_load(init/0).

-export([make_counter/0,
         inc_counter/1,
         value/1,
         reset/1,
         run_single_counter/0]).

-define(APPNAME, prometheus).
-define(LIBNAME, 'prometheus.erl').


-define(COUNT, 3 * 1000*1000).
-define(TIMEOUT, 100*1000).
-define(MILLION, 1000*1000).
-define(MILLIONS(C), C * ?MILLION).

make_counter() ->
  not_loaded(?LINE).

inc_counter(_Counter) ->
  not_loaded(?LINE).

value(_Counter) ->
  not_loaded(?LINE).

reset(_Counter) ->
  not_loaded(?LINE).

init() ->
  SoName = case code:priv_dir(?APPNAME) of
             {error, bad_name} ->
               case filelib:is_dir(filename:join(["..", priv])) of
                 true ->
                   filename:join(["..", priv, ?LIBNAME]);
                 _ ->
                   filename:join([priv, ?LIBNAME])
               end;
             Dir ->
               filename:join(Dir, ?LIBNAME)
           end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).


run_single_counter() ->
  C = make_counter(),

  run_test(C, 1, ?MILLIONS(1), ?MILLIONS(1)),
  run_test(C, 5, ?MILLIONS(1), ?MILLIONS(6)),
  run_test(C, 10, ?MILLIONS(1), ?MILLIONS(16)),
  run_test(C, 100, ?MILLIONS(1), ?MILLIONS(116)),

  io:format("Finished~n").

run_test(C, Pids, Count, ExpectedValue) ->
  io:format("~s~n"
            "  Pids: ~p~n"
            "  Iterations: ~p~n"
            "  Expected Value: ~p~n"
            "  Duration (sec): --",
            [color:blueb("Running single counter benchmark:"),
             Pids, Count, ExpectedValue]),
  Time = run_int(C, Pids, Count),
  Value = case value(C) of
            CValue when CValue == ExpectedValue -> color:greenb(io_lib:format("~p", [ExpectedValue]));
            CValue -> color:redb(io_lib:format("~p", [CValue]))
          end,
  io:format("  Counter value: ~s~n", [Value]),
  {Time, value(C)}.

run_int(C, PCount, Count) ->
  StartTime = erlang:monotonic_time(),
  Mgr = self(),
  Printer = spawn_link(fun() -> printer_loop(StartTime, PCount * Count) end),
  Pids = [spawn_link(fun() -> loop(Mgr, Printer, Count, C) end) || _ <- lists:seq(1, PCount)],
  collect(Printer, Pids).

collect(Printer, []) ->
  EndTime = erlang:monotonic_time(),
  Printer ! {stop, self()},
  receive
    _ ->
      ok
  after 5000 ->
      ok
  end,
  EndTime;
collect(Printer, [Pid | Pids]) ->
  receive
    {ok, Pid} ->
      collect(Printer, Pids)
  after ?TIMEOUT ->
      {error, timeout}
  end.

printer_loop(StartTime, TotalCount) ->
  printer_loop(StartTime, TotalCount, 0).

printer_loop(StartTime, TotalCount, IncCount) ->
  receive
    ten ->
      Flushes = flush(1),
      print_duration(StartTime, TotalCount, IncCount + 250000 * Flushes),
      printer_loop(StartTime, TotalCount, IncCount + 250000 * Flushes);
    {stop, Mgr} ->
      io:format("\n"),
      Mgr ! printer_stopped
  after ?TIMEOUT ->
      {error, timeout}
  end.

flush(Flushes) ->
  receive
    ten -> flush(Flushes + 1)
  after 0 ->
      Flushes
  end.

print_duration(StartTime, TotalCount, IncCount) ->
  CurrentTime = erlang:monotonic_time(),
  TimeDiff = erlang:convert_time_unit(CurrentTime - StartTime, native,  milli_seconds) / 1000,
  io:format("\r  Duration (sec): ~p (~.1f%)", [TimeDiff, (IncCount/TotalCount)*100]).

loop(Mgr, Printer, 0, _Name) ->
  Printer ! ten,
  Mgr ! {ok, self()};
loop(Mgr, Printer, N, Name) ->
  case N of
    %% 100000 ->
    %%   Printer ! ten;
    250000 ->
      Printer ! ten;
    %% 300000 ->
    %%   Printer ! ten;
    %% 400000 ->
    %%   Printer ! ten;
    500000 ->
      Printer ! ten;
    %% 600000 ->
    %%   Printer ! ten;
    750000 ->
      Printer ! ten;
    %% 800000 ->
    %%   Printer ! ten;
    %% 900000 ->
    %%   Printer ! ten;
    _ -> ok
  end,
  inc_counter(Name),
  loop(Mgr, Printer, N-1, Name).
