-module(sample_budget_one_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_budget_one_selected/0, prop_budget_one_hidden/0,
         property_weight/2, eqc_module_weight/1]).

prop_budget_one_selected() ->
    ?FORALL(N, int(),
            begin
                timer:sleep(200),
                N =:= N
            end).

prop_budget_one_hidden() ->
    ?FORALL(N, int(),
            begin
                timer:sleep(200),
                N =:= N
            end).

property_weight("focused", prop_budget_one_selected) ->
    1;
property_weight("focused", prop_budget_one_hidden) ->
    0;
property_weight(_, _) ->
    1.

eqc_module_weight("focused") ->
    1;
eqc_module_weight(_) ->
    1.
