-module(sample_fail_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_broken/0]).

prop_broken() ->
    ?FORALL(N, int(), N + 1 =:= N).
