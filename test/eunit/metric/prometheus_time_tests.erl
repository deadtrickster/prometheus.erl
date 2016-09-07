-module(prometheus_time_tests).

-include_lib("eunit/include/eunit.hrl").

from_native_test() ->
  NativeInUS = erlang:convert_time_unit(1, micro_seconds, native),
  ?assertEqual(2.5, prometheus_time:from_native(2.5 * NativeInUS, microseconds)),
  ?assertEqual(2.6, prometheus_time:from_native(2.6 * 1000 * NativeInUS, milliseconds)),
  ?assertEqual(2.23, prometheus_time:from_native(2.23 * 1000000 * NativeInUS, seconds)),
  ?assertEqual(3.4, prometheus_time:from_native(3.4 * 60 * 1000000 * NativeInUS,
                                                minutes)),
  ?assertEqual(0.4, prometheus_time:from_native(0.4 * 3600 * 1000000 * NativeInUS,
                                                hours)),
  ?assertEqual(0.1, prometheus_time:from_native(0.1 * 86400 * 1000000 * NativeInUS,
                                                days)).

to_native_test() ->
  NativeInUS = erlang:convert_time_unit(1, micro_seconds, native),
  ?assertEqual(trunc(2.43 * NativeInUS), prometheus_time:to_native(2.43, microseconds)),
  ?assertEqual(trunc(2.6 * 1000 * NativeInUS),
               prometheus_time:to_native(2.6, milliseconds)),
  ?assertEqual(trunc(2.23 * 1000000 * NativeInUS),
               prometheus_time:to_native(2.23, seconds)),
  ?assertEqual(trunc(3.4 * 60 * 1000000 * NativeInUS), prometheus_time:to_native(3.4,
                                                                          minutes)),
  ?assertEqual(trunc(0.4 * 3600 * 1000000 * NativeInUS), prometheus_time:to_native(0.4,
                                                                            hours)),
  ?assertEqual(trunc(0.1 * 86400 * 1000000 * NativeInUS), prometheus_time:to_native(0.1,
                                                                             days)).
