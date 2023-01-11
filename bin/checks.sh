#!/usr/bin/env sh
mkdir -p _build
if [ ! -e ./_build/rebar3 ]; then
    curl -L -o ./_build/rebar3 https://s3.amazonaws.com/rebar3/rebar3
    chmod +x ./_build/rebar3
fi

if [ ! -e ./_build/elvis ]; then
  (
      mkdir -p _build/elvis-build
      cd _build/elvis-build
      curl -L -o elvis.tar.gz https://github.com/inaka/elvis/archive/refs/tags/1.1.0.tar.gz
      tar -x --strip-components=1 -z -f elvis.tar.gz
      ../rebar3 escriptize
      cp _build/default/bin/elvis ..
  )
fi

./_build/elvis rock && ./_build/rebar3 do xref, dialyzer, eunit
