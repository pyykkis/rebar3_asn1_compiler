-module(rebar3_asn1_compiler).
-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Commands = [fun asn1_prv_compile:init/1, fun asn1_prv_clean:init/1],
    lists:foldl(fun (F, {ok, S}) -> F(S) end, {ok, State}, Commands).
