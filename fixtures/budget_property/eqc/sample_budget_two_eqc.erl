-module(sample_budget_two_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_budget_two_selected/0, prop_budget_two_hidden/0, property_weight/2]).

prop_budget_two_selected() ->
    ?FORALL(N, int(),
            begin
                timer:sleep(200),
                N =:= N
            end).

prop_budget_two_hidden() ->
    ?FORALL(N, int(),
            begin
                timer:sleep(200),
                N =:= N
            end).

property_weight("focused", prop_budget_two_selected) ->
    1;
property_weight("focused", prop_budget_two_hidden) ->
    0;
property_weight(_, _) ->
    1.
