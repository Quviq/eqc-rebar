-module(sample_budget_two_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_budget_two/0]).

prop_budget_two() ->
    ?FORALL(N, int(),
            begin
                timer:sleep(200),
                N =:= N
            end).
