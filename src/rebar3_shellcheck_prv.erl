-module(rebar3_shellcheck_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, shellcheck).
-define(DEPS, []).

-type path() :: string().
-type glob_pattern() :: string().

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 shellcheck --directory \"scripts\" --format tty --pattern \"*.{sh,bash}\""},
                                 {opts, options()},
                                 {short_desc, "Runs 'shellcheck' on shell scripts"},
                                 {desc, "Runs the program 'shellcheck' on the given shell scripts."}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    check_shellcheck_availability(),
    {Arguments, _} = rebar_state:command_parsed_args(State),
    Path = proplists:get_value(directory, Arguments),
    Pattern = proplists:get_value(pattern, Arguments),
    Files = find_source_files(Path, Pattern),
    Response = run_shellcheck(Files, Arguments),
    handle_shellcheck_response(Response, State).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    rebar_api:error("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

check_shellcheck_availability() ->
    case os:find_executable("shellcheck") of
        false ->
            rebar_api:warn("shellcheck is not installed or is not in your PATH.", []),
            ok;
        _ ->
            ok
    end.

-spec options() -> list().
options() ->
    [{directory, $d, "directory", {string, "."}, "the directory where to search for the scripts"},
     {pattern, $p, "pattern", {string, "**/*.sh"}, "the glob pattern that will be used to find the scripts"},
     {format, $f, "format", {string, "gcc"}, "the format that will be used for the errors (checkstyle, gcc, json, tty)"}
    ].

-spec find_source_files(path(), glob_pattern()) -> [path()].
find_source_files(Path, Pattern) ->
    [filename:join(Path, File) || File <- filelib:wildcard(Pattern, Path)].

-spec run_shellcheck([path()], [{atom(), term()}]) -> {ok, term()} | {error, term()}.
run_shellcheck(Files, Options) ->
    RunLinter = run_linter(Options),
    F = fun (File, Accumulator) ->
                handle_linter_response(RunLinter, File, Accumulator)
        end,
    lists:foldl(F, {ok, []}, Files).

run_linter(Options) ->
    Format = proplists:get_value(format, Options),
    fun (File) ->
            Command = io_lib:format("shellcheck --format=~s --color=always ~s", [Format, File]),
            Port = erlang:open_port({spawn, Command}, [hide, exit_status]),
            wait_for_response(Port, [])
    end.

wait_for_response(Port, Data) ->
    receive
        {Port, {exit_status, 0}} -> {ok, Data};
        {Port, {exit_status, _}} -> {error, Data};
        {Port, {data, NewData}} ->
            wait_for_response(Port, Data ++ NewData)
    end.

handle_linter_response(RunLinter, File, {error, Data}) ->
    {_, NewData} = RunLinter(File),
    {error, Data ++ NewData};
handle_linter_response(RunLinter, File, {ok, Data}) ->
    {Status, NewData} = RunLinter(File),
    {Status, Data ++ NewData}.

handle_shellcheck_response({ok, Data}, State) ->
    rebar_api:console("~s", [Data]),
    {ok, State};
handle_shellcheck_response({error, Data}, _) ->
    rebar_api:console("~s", [Data]),
    {error, "Shellcheck found some warnings/errors."}.
