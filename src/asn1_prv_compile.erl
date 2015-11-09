-module(asn1_prv_compile).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [{default, app_discovery}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},                % The 'user friendly' name of the task
            {module, ?MODULE},                % The module implementation of the task
            {bare, true},                     % The task can be run by the user, always true
            {deps, ?DEPS},                    % The list of dependencies
            {example, "rebar3 asn1 compile"}, % How to use the plugin
            {opts, []},                       % list of options understood by the plugin
            {short_desc, "Compile ASN.1 with Rebar3"},
            {desc, "Compile ASN.1 with Rebar3"},
            {namespace, asn1}
    ]),

    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    AppPaths = lists:map(fun rebar_app_info:dir/1, rebar_state:project_apps(State)),
    lists:foreach(do_compile(State), AppPaths),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_compile(State) ->
    DefaultOpts = [{asndir, "asn1"}, {outdir, "asn1_gen"}, {noobj, true}],
    UserOpts    = proplists:unfold(rebar_state:get(State, asn1_opts, [])),
    Opts        = proplists:compact(orddict:from_list(lists:append(DefaultOpts, UserOpts))),

    fun (AppPath) ->
            AsnDir   = filename:join(AppPath, proplists:get_value(asndir, Opts)),
            AsnFiles = filelib:wildcard(filename:join([AppPath, AsnDir, "*.asn1"])),
            OutDir   = filename:join(AppPath, proplists:get_value(outdir, Opts)),

            file:make_dir(OutDir),
            lists:foreach(compile_asn_file(Opts), lists:filter(needs_compile(OutDir), AsnFiles))
    end.

compile_asn_file(Opts) ->
    fun (AsnFile) ->
            rebar_api:info("Generating parser for: ~p", [AsnFile]),
            asn1ct:compile(AsnFile, Opts)
    end.

needs_compile(OutDir) ->
    fun (Source) ->
            Target = filename:join(OutDir, filename:basename(Source, ".asn1") ++ ".erl"),
            filelib:last_modified(Source) > filelib:last_modified(Target)
    end.
