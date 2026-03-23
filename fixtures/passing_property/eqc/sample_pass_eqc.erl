-module(sample_pass_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_reverse_reverse/0]).

prop_reverse_reverse() ->
    ?FORALL(L, list(int()), lists:reverse(lists:reverse(L)) =:= L).
