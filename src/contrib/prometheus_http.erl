%% @doc HTTP instrumentation helpers
-module(prometheus_http).

-export([microseconds_duration_buckets/0,
         status_class/1,
         negotiate/2]).

-ifdef(TEST).
-export([parse_accept/1]).
-endif.

-export_type([status_code/0,
              status_class/0]).

%%====================================================================
%% Types
%%====================================================================

-type status_code() :: pos_integer().
-type status_class() :: prometheus_model_helpers:label_value().

-record(media_range, {type,
                      subtype,
                      q,
                      params}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc
%% Returns default microseconds buckets for measuring http requests duration.
%%
%% <pre lang="erlang">
%% 1> prometheus_http:microseconds_duration_buckets().
%% [10, 25, 50, 100, 250, 500,
%%  1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
%%  1000000, 2500000, 5000000, 10000000]
%% </pre>
%% @end
-spec microseconds_duration_buckets() -> prometheus_buckets:buckets().
microseconds_duration_buckets() ->
  [10, 25, 50, 100, 250, 500,
   1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
   1000000, 2500000, 5000000, 10000000].

%% @doc
%% Returns status class for the http status code `SCode'.
%%
%% <pre lang="erlang">
%% 2> prometheus_http:status_class(202).
%% "success"
%% </pre>
%%
%% Raises `{invalid_value_error, SCode, Message}' error if `SCode'
%% isn't a positive integer.
%% @end
-spec status_class(SCode) -> StatusClass when
    SCode :: status_code(),
    StatusClass :: status_class().
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 100 ->
  "unknown";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 200 ->
  "informational";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 300 ->
  "success";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 400 ->
  "redirection";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 500 ->
  "client-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 600 ->
  "server-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode >= 600 ->
  "unknown";
status_class(C) ->
  erlang:error({invalid_value, C, "status code must be a positive integer"}).


%% @doc
%% Negotiate the most appropriate content_type given the accept header
%% and a list of alternatives.
%% @end
-spec negotiate(Header, Alternatives) -> Match when
    Header :: BinaryOrString,
    Alternatives :: [Alternative],
    Alternative :: BinaryOrString | {BinaryOrString | Tag},
    BinaryOrString :: binary() | string(),
    Tag :: any(),
    Match :: Tag | nomatch.
negotiate(Header, Alternatives) ->

  MediaRanges = parse_accept(ensure_string(Header)),

  Alts = lists:map(fun (Alt) ->
                       {A, Tag} = case Alt of
                                    {_, _} -> Alt;
                                    _ -> {Alt, Alt}
                                  end,
                       PA = parse_media_range(ensure_string(A)),
                       %% list of Alt-MR scores
                       AltMRScores = lists:map(fun (MR) ->
                                                   {score_alt(MR, PA), MR}
                                               end,
                                               MediaRanges),
                       %% best Media Range match for this Alternative
                       [{Score, BMR} | _ ] = lists:sort(fun scored_cmp/2,
                                                        AltMRScores),
                       case Score of
                         0 ->
                           {-1, Tag};
                         _ ->
                           #media_range{q = BMRQ} = BMR,
                           {BMRQ, Tag}
                       end
                   end,
                   Alternatives),

  %% find alternative with the best score
  %% keysort is stable so order of Alternatives preserved
  %% after sorting Tail has the best score.
  %% However if multiple alternatives have the same score as Tail
  %% we should find first best alternative to respect user's priority.
  {_, Tag} = find_preferred_best(lists:keysort(1, Alts)),
  Tag.

%%====================================================================
%% Private Parts
%%====================================================================

parse_accept(AcceptString) ->
  lists:map(fun parse_media_range/1,
            string:tokens(AcceptString, ",")).

parse_media_range(RawMRString) ->
  [MR | RawParams] = string:tokens(RawMRString, ";"),

  [Type, Subtype] = lists:map(fun string:strip/1, string:tokens(MR, "/")),

  Params = lists:filtermap(fun parse_media_range_param/1, RawParams),

  {Q, ParamsWOQ} = find_media_range_q(Params),

  #media_range{type = Type,
               subtype = Subtype,
               q = Q,
               params = ParamsWOQ}.

parse_media_range_param(Param) ->
  case string:tokens(Param, "=") of
    [Name, Value] -> {true, {Name, Value}};
    _ -> false %% simply ignore malformed Name=Value pairs
  end.

find_media_range_q(Params) ->
  {parse_q(proplists:get_value("q", Params, "1")),
   proplists:delete("q", Params)}.

parse_q(Q) ->
  try
    case lists:member($., Q) of
      true ->
        list_to_float(Q);
      false ->
        list_to_integer(Q)
    end
  catch error:badarg ->
      0
  end.

scored_cmp({S1, _}, {S2, _}) ->
  S1 > S2.

find_preferred_best(Sorted) ->
  [B | R] = lists:reverse(Sorted),
  find_preferred_best(B, R).

find_preferred_best({Q, _}, [{Q, _} = H | R]) ->
  find_preferred_best(H, R);
find_preferred_best(B, []) ->
  B;
find_preferred_best(B, _) ->
  B.


%% Alternative "text/plain; version=4"
%% text/plain; version=4 > text/plan > text/plain; n=v > text/* > */* > image/*
score_alt(#media_range{type = Type,
                       subtype = SubType,
                       params = MRParams},
          #media_range{type = Type,
                       subtype = SubType,
                       params = AltParams}) ->
  8 + 4 + score_params(MRParams, AltParams);
score_alt(#media_range{type = Type,
                       subtype = "*"},
          #media_range{type = Type}) ->
  8 + 3;
score_alt(#media_range{type = "*"},
          _) ->
  8;
score_alt(_, _) ->
  0.

%% If media range doesn't have params 1
%% If params match 2
%% otherwise 0
score_params([], _) ->
  1;
score_params(MRParams, AltParams) when length(MRParams) == length(AltParams) ->
  case lists:sort(MRParams) == lists:sort(AltParams) of
    true -> 2;
    _ -> 0
  end;
score_params(_, _) ->
  0.

ensure_string(V) when is_list(V) ->
  V;
ensure_string(V) when is_binary(V) ->
  binary_to_list(V).
