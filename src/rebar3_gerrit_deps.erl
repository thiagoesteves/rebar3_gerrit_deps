-module(rebar3_gerrit_deps).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok,rebar_state:add_resource(State, {gerrit, rebar_gerrit})}.
