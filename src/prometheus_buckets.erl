-module(prometheus_buckets).

-export([generate_linear/3,
         generate_exponential/3]).

%%====================================================================
%% Public API
%%====================================================================

generate_linear(_Start, _Step, Count) when Count < 1 ->
  erlang:error({invalid_value, Count, "Buckets count should be positive"});
generate_linear(Start, Step, Count) ->
  [try_to_maintain_integer_bounds(Bound) || Bound <- lists:seq(Start, Start + Step*(Count - 1), Step)].

generate_exponential(_Start, _Factor, Count) when Count < 1 ->
  erlang:error({invalid_value, Count, "Buckets count should be positive"});
generate_exponential(Start, _Factor, _Count) when Start =< 0 ->
  erlang:error({invalid_value, Start, "Buckets start should be positive"});
generate_exponential(_Start, Factor, _Count) when Factor =< 1 ->
  erlang:error({invalid_value, Factor, "Buckets factor should be greater than 1"});
generate_exponential(Start, Factor, Count) ->
  [try_to_maintain_integer_bounds(Start*math:pow(Factor, I)) || I <- lists:seq(0, Count-1)].

%%====================================================================
%% Private Parts
%%====================================================================

try_to_maintain_integer_bounds(Bound) when is_integer(Bound) ->
  Bound;
try_to_maintain_integer_bounds(Bound) when is_float(Bound) ->
  TBound = trunc(Bound),
  case TBound == Bound of
    true -> TBound;
    false -> Bound
  end.
