-module(sample_pass_in_test_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_test_dir_property/0]).

prop_test_dir_property() ->
    ?FORALL(N, int(), N =:= N).
