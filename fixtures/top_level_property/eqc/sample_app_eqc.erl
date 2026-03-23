-module(sample_app_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_top_level/0]).

prop_top_level() ->
    ?FORALL(N, int(), N =:= N).
