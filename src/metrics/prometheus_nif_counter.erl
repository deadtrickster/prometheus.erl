-module(prometheus_nif_counter).
-on_load(init/0).

-export([new/0,
         new/1,
         inc/1,
         inc/2,
         value/1,
         reset/1]).

-define(APPNAME, prometheus).
-define(LIBNAME, 'prometheus.erl').


-define(COUNT, 3 * 1000*1000).
-define(TIMEOUT, 100*1000).
-define(MILLION, 1000*1000).
-define(MILLIONS(C), C * ?MILLION).

new() ->
  new(0).

new(_Counter) ->
  not_loaded(?LINE).

inc(Counter) ->
  inc(Counter, 1).

inc(_Counter, _Value) ->
  not_loaded(?LINE).

value(_Counter) ->
  not_loaded(?LINE).

reset(Counter) ->
  set(Counter, 0).

set(_Counter, _Value) ->
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
