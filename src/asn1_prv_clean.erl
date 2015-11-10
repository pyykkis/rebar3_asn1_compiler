-module(asn1_prv_clean).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'clean').
-define(DEPS, [{default, app_discovery}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},              % The 'user friendly' name of the task
            {module, ?MODULE},              % The module implementation of the task
            {bare, false},                  % The task can be run by the user, always true
            {deps, ?DEPS},                  % The list of dependencies
            {example, "rebar3 asn1 clean"}, % How to use the plugin
            {opts, []},                     % list of options understood by the plugin
            {short_desc, "Clean ASN.1 with Rebar3"},
            {desc, "Clean ASN.1 with Rebar3"},
            {namespace, asn1}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar3_asn1_compiler:get_target_apps(State),
    AppPaths = lists:map(fun rebar_app_info:dir/1, Apps),
    lists:foreach(do_clean(State), AppPaths),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) -> 
    io_lib:format("~p", [Reason]).

do_clean(State) ->
    {_Args, _} = rebar_state:command_parsed_args(State),
    GenPath   = "asn1_gen",

    fun (AppPath) ->
            rebar_api:info("Cleaning ASN.1 generated files.", []),
            GeneratedFiles = filelib:wildcard(filename:join([AppPath, GenPath, "*.*"])),
            lists:foreach(fun file:delete/1, GeneratedFiles)
    end.
