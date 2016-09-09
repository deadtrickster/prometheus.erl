%% @hidden
-module(prometheus_time).

-export([duration_unit_from_string/1,
         validate_duration_unit/1,
         maybe_convert_to_native/2,
         maybe_convert_to_du/2]).

-ifdef(TEST).
-export([from_native/2,
         to_native/2]).
-endif.

%%====================================================================
%% Macros
%%====================================================================

-define(DURATION_UNITS, [{"microseconds", microseconds},
                         {"milliseconds", milliseconds},
                         {"seconds", seconds},
                         {"minutes", minutes},
                         {"hours", hours},
                         {"days", days}]).

%%====================================================================
%% Public API
%%====================================================================

duration_unit_from_string(Str) ->
  duration_unit_from_string(Str, ?DURATION_UNITS).

validate_duration_unit(false) ->
  false;
validate_duration_unit(undefined) ->
  undefined;
validate_duration_unit(SDU) ->
  case lists:any(fun({_, DU}) ->
                     DU == SDU
                 end,
                 ?DURATION_UNITS) of
    true ->
      SDU;
    _ ->
      erlang:error({invalid_value, SDU, "unknown duration unit"})
  end.

maybe_convert_to_native(_, infinity) ->
  infinity;
maybe_convert_to_native(DU, Value) ->
  case DU of
    undefined -> Value;
    _ -> to_native(Value, DU)
  end.

maybe_convert_to_du(_, infinity) ->
  infinity;
maybe_convert_to_du(DU, Value) ->
  case DU of
    undefined -> Value;
    _ -> from_native(Value, DU)
  end.

%%====================================================================
%% Private Parts
%%====================================================================

duration_unit_from_string(Str, [{SDU, DU}|Rest]) ->
  case string:rstr(Str, SDU) of
    0 -> duration_unit_from_string(Str, Rest);
    _ -> DU
  end;
duration_unit_from_string(_, []) ->
  undefined.

from_native(Value) ->
  erlang:convert_time_unit(trunc(Value), native, nano_seconds).

from_native(Value, microseconds) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 1000;
from_native(Value, milliseconds) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 1000000;
from_native(Value, seconds) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 1000000000;
from_native(Value, minutes) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 60000000000;
from_native(Value, hours) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 3600000000000;
from_native(Value, days) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 86400000000000.

to_native(Value) ->
  erlang:convert_time_unit(trunc(Value), nano_seconds, native).

to_native(Value, microseconds) ->
  to_native(Value * 1000);
to_native(Value, milliseconds) ->
  to_native(Value * 1000000);
to_native(Value, seconds) ->
  to_native(Value * 1000000000);
to_native(Value, minutes) ->
  to_native(Value * 60000000000);
to_native(Value, hours) ->
  to_native(Value * 3600000000000);
to_native(Value, days) ->
  to_native(Value * 86400000000000).
