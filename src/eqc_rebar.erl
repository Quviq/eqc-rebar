-module(eqc_rebar).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    eqc_rebar_prv:init(State).
