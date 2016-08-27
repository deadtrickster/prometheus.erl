-module(prometheus_buckets).

-export([default/0,
         linear/3,
         exponential/3]).

-export_type([bucket_bound/0,
              buckets/0]).

%%====================================================================
%% Types
%%====================================================================

-type bucket_bound() :: number() | infinity.
-type buckets() :: [bucket_bound(), ...].

%%====================================================================
%% Public API
%%====================================================================

-spec default() -> buckets().
default() -> [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10].

-spec linear(integer(), pos_integer(), pos_integer()) -> buckets().
linear(_Start, _Step, Count) when Count < 1 ->
  erlang:error({invalid_value, Count, "Buckets count should be positive"});
linear(Start, Step, Count) ->
  Bounds = lists:seq(Start, Start + Step*(Count - 1), Step),
  [try_to_maintain_integer_bounds(Bound) || Bound <- Bounds].

-spec exponential(number(), number(), pos_integer()) -> buckets().
exponential(_Start, _Factor, Count) when Count < 1 ->
  erlang:error({invalid_value, Count, "Buckets count should be positive"});
exponential(Start, _Factor, _Count) when Start =< 0 ->
  erlang:error({invalid_value, Start, "Buckets start should be positive"});
exponential(_Start, Factor, _Count) when Factor =< 1 ->
  erlang:error({invalid_value, Factor,
                "Buckets factor should be greater than 1"});
exponential(Start, Factor, Count) ->
  [try_to_maintain_integer_bounds(Start*math:pow(Factor, I)) ||
    I <- lists:seq(0, Count-1)].

%%====================================================================
%% Private Parts
%%====================================================================

-spec try_to_maintain_integer_bounds(integer()) -> integer();
                                    (float())   -> integer() | float().
try_to_maintain_integer_bounds(Bound) when is_integer(Bound) -> Bound;
try_to_maintain_integer_bounds(Bound) when is_float(Bound) ->
  TBound = trunc(Bound),
  case TBound == Bound of
    true  -> TBound;
    false -> Bound
  end.
