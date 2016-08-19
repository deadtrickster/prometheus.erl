-module(prometheus_instrumenter_tests).

-include_lib("eunit/include/eunit.hrl").

instrumenter_setup_test() ->
  ?assertNotMatch(undefined, ets:info(prometheus_instrumenter_tests)).
