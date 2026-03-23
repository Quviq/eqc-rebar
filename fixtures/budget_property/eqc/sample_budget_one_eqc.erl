-module(sample_budget_one_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_budget_one/0]).

prop_budget_one() ->
    ?FORALL(N, int(),
            begin
                timer:sleep(200),
                N =:= N
            end).
