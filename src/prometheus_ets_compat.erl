-module(prometheus_ets_compat).

-export([take/2]).

%% ===================================================================
%% API
%% ===================================================================

take(Tab, Key) ->
  try 
    ets:take(Tab, Key)
  catch
    error:undef ->
      ets_take_fallback(Tab, Key)
  end.

%% ===================================================================
%% Private functions
%% ===================================================================

ets_take_fallback(Tab, Key) ->
  Items = ets:lookup(Tab, Key),
  ets:delete(Tab, Key),
  Items.
