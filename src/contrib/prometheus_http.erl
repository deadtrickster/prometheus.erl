%% @doc HTTP instrumentation helpers
-module(prometheus_http).

-export([microseconds_duration_buckets/0,
         status_class/1,
         negotiate/2]).

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
%%
%% Ported from [goautoneg](https://bitbucket.org/ww/goautoneg).
%% ```
%% Copyright (c) 2011, Open Knowledge Foundation Ltd.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:
%%
%%     Redistributions of source code must retain the above copyright
%%     notice, this list of conditions and the following disclaimer.
%%
%%     Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in
%%     the documentation and/or other materials provided with the
%%     distribution.
%%
%%     Neither the name of the Open Knowledge Foundation Ltd. nor the
%%     names of its contributors may be used to endorse or promote
%%     products derived from this software without specific prior written
%%     permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% '''
%% @end
-spec negotiate(Header, Alternatives) -> Match when
    Header :: BinaryOrString,
    Alternatives :: [Alternative],
    Alternative :: BinaryOrString | {BinaryOrString | Tag},
    BinaryOrString :: binary() | string(),
    Tag :: any(),
    Match :: Tag | nomatch.
negotiate(Header, Alternatives) ->
  Alts = lists:map(fun (Alt) ->
                       {A, Tag} = case Alt of
                                    {_, _} -> Alt;
                                    _ -> {Alt, Alt}
                                  end,
                       {parse_media_range(ensure_string(A)), Tag}
                   end,
                   Alternatives),

  MediaRanges = parse_accept(ensure_string(Header)),

  try_match_mr(MediaRanges, Alts).



%%====================================================================
%% Private Parts
%%====================================================================

try_match(Match, [E | Rest]) ->
  case Match(E) of
    nomatch -> try_match(Match, Rest);
    V -> V
  end;
try_match(_Match, []) -> nomatch.

try_match_mr(MRs, Alts) ->
  try_match(fun (MR) ->
                try_match_alt(MR, Alts)
            end,
            MRs).

try_match_alt(MR, Alts) ->
  TryMatchAlt = fun(Ctsp) ->
                    {A, Tag} = Ctsp,
                    case mr_match(MR, A) of
                      true -> Tag;
                      _  -> nomatch
                    end
                end,
  try_match(TryMatchAlt, Alts).

mr_match(MR, #media_range{type = Type,
                          subtype = Subtype}) ->
  case MR of
    #media_range{type = Type,
                 subtype = Subtype} ->
      true;
    #media_range{type = Type,
                 subtype = "*"} ->
      true;
    #media_range{type = "*",
                 subtype = "*"} ->
      true;
    _ ->
      false
  end.

parse_accept(AcceptString) ->
  lists:sort(fun compare_media_ranges/2,
             lists:map(fun parse_media_range/1,
                       string:tokens(AcceptString, ","))).

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

compare_media_ranges(#media_range{q = Q1} = Range1,
                     #media_range{q = Q2} = Range2) ->

  case Q1 of
    _ when Q1 > Q2 ->
      true;
    Q2 ->
      compare_media_types(Range1, Range2);
    _ ->
      false
  end.

compare_media_types(#media_range{type = "*",
                                 params = Params1},
                    #media_range{type = "*",
                                 params = Params2}) ->
  length(Params1) >= length(Params2);
compare_media_types(#media_range{type = "*"},
                    _) ->
  false;
compare_media_types(_,
                    #media_range{type = "*"}) ->
  true;
compare_media_types(#media_range{subtype = "*",
                                 params = Params1},
                    #media_range{subtype = "*",
                                 params = Params2}) ->
  length(Params1) =< length(Params2);
compare_media_types(#media_range{subtype = "*"},
                    _) ->
  false;
compare_media_types(_,
                    #media_range{subtype = "*"}) ->
  true;
compare_media_types(#media_range{params = Params1},
                    #media_range{params = Params2}) ->
  length(Params1) >= length(Params2).

ensure_string(V) when is_list(V) ->
  V;
ensure_string(V) when is_binary(V) ->
  binary_to_list(V).
