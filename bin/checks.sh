#!/bin/sh

./elvis rock && rebar compile && rebar3 do xref, dialyzer, eunit
