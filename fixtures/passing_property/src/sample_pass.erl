-module(sample_pass).

-export([covered/0, uncovered/0]).

covered() ->
    ok.

uncovered() ->
    not_covered.
