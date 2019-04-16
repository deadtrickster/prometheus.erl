#!/bin/sh
mkdir _build
curl -L -o ./_build/rebar3 https://s3.amazonaws.com/rebar3/rebar3
chmod +x ./_build/rebar3
./elvis rock && ./_build/rebar3 do xref, dialyzer, eunit
