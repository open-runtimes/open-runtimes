-module(configure).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = configure_prv:init(State),
    {ok, State1}.
