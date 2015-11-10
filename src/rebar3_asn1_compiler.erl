-module(rebar3_asn1_compiler).
-export([init/1, get_target_apps/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Commands   = [fun asn1_prv_compile:init/1, fun asn1_prv_clean:init/1],
    lists:foldl(fun (F, {ok, S}) -> F(S) end, {ok, State}, Commands).

get_target_apps(State) ->
    case rebar_state:current_app(State) of
        %% Process all the apps in the release
        undefined -> rebar_state:project_apps(State);
        %% Called from a hook, process only the current app
        AppInfo   -> [AppInfo]
    end.
