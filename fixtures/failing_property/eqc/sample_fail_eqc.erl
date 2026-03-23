-module(sample_fail_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_safe/0, prop_broken/0, property_weight/2]).

prop_safe() ->
    ?FORALL(N, int(), N =:= N).

prop_broken() ->
    ?FORALL(N, int(), N + 1 =:= N).

property_weight("focused", prop_safe) ->
    1;
property_weight("focused", prop_broken) ->
    0;
property_weight(_, _) ->
    1.
