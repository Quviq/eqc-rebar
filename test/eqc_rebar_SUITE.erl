-module(eqc_rebar_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    help_eqc_from_snapshot/1
]).

all() ->
    [help_eqc_from_snapshot].

init_per_suite(Config) ->
    RepoRoot = find_repo_root(filename:dirname(code:which(?MODULE))),
    [{repo_root, RepoRoot} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    TmpRoot = make_temp_dir("eqc-rebar-ct"),
    [{tmp_root, TmpRoot} | Config].

end_per_testcase(_TestCase, Config) ->
    TmpRoot = ?config(tmp_root, Config),
    _ = file:del_dir_r(TmpRoot),
    ok.

help_eqc_from_snapshot(Config) ->
    RepoRoot = ?config(repo_root, Config),
    TmpRoot = ?config(tmp_root, Config),
    SnapshotDir = filename:join(TmpRoot, "plugin"),
    FixtureTemplateDir = filename:join([RepoRoot, "fixtures", "help_eqc"]),
    FixtureDir = filename:join(TmpRoot, "fixture"),

    ok = snapshot_repo(RepoRoot, SnapshotDir),
    ok = copy_tree(FixtureTemplateDir, FixtureDir),
    ok = inject_plugin_url(filename:join(FixtureDir, "rebar.config"),
                           "file://" ++ SnapshotDir),

    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 help eqc"),
    case {Status, contains(Output, "Usage: rebar3 eqc"),
                  contains(Output, "Plugin to enable running eqc on QuickCheck properties")} of
        {0, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_rebar3_output, Status, Output})
    end.

make_temp_dir(Prefix) ->
    Base = case os:getenv("TMPDIR") of
               false -> "/tmp";
               TmpDir -> TmpDir
           end,
    Dir = filename:join(Base, Prefix ++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    Dir.

snapshot_repo(RepoRoot, SnapshotDir) ->
    Command = lists:flatten(
                io_lib:format(
                  "mkdir -p ~s && "
                  "tar -C ~s --exclude .git --exclude _build --exclude .rebar3 --exclude _checkouts -cf - . | "
                  "tar -C ~s -xf - && "
                  "cd ~s && "
                  "git init -b master >/dev/null && "
                  "git config user.email 'ct@example.invalid' && "
                  "git config user.name 'CT Snapshot' && "
                  "git add -A && "
                  "git commit -m 'test snapshot' >/dev/null",
                  [sh_quote(SnapshotDir),
                   sh_quote(RepoRoot),
                   sh_quote(SnapshotDir),
                   sh_quote(SnapshotDir)])),
    assert_shell_success(run_shell("/", Command)).

copy_tree(SourceDir, DestDir) ->
    Command = lists:flatten(
                io_lib:format(
                  "mkdir -p ~s && tar -C ~s -cf - . | tar -C ~s -xf -",
                  [sh_quote(DestDir), sh_quote(SourceDir), sh_quote(DestDir)])),
    assert_shell_success(run_shell("/", Command)).

inject_plugin_url(RebarConfigPath, PluginUrl) ->
    {ok, Contents} = file:read_file(RebarConfigPath),
    Updated = binary:replace(Contents, <<"__PLUGIN_URL__">>,
                             unicode:characters_to_binary(PluginUrl), [global]),
    ok = file:write_file(RebarConfigPath, Updated).

assert_shell_success(#{status := 0}) ->
    ok;
assert_shell_success(#{status := Status, output := Output}) ->
    ct:fail({command_failed, Status, Output}).

run_shell(Cwd, Command) ->
    Port = open_port({spawn_executable, "/bin/sh"},
                     [{args, ["-lc", Command]},
                      {cd, Cwd},
                      binary,
                      exit_status,
                      use_stdio,
                      stderr_to_stdout,
                      eof]),
    collect_port_output(Port, []).

collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, [Data | Acc]);
        {Port, eof} ->
            receive
                {Port, {exit_status, Status}} ->
                    #{status => Status,
                      output => unicode:characters_to_list(iolist_to_binary(lists:reverse(Acc)))}
            end
    end.

contains(Haystack, Needle) ->
    binary:match(unicode:characters_to_binary(Haystack),
                 unicode:characters_to_binary(Needle)) =/= nomatch.

find_repo_root(Dir) ->
    case {filelib:is_file(filename:join(Dir, "rebar.config")),
          filelib:is_file(filename:join([Dir, "src", "eqc_rebar.erl"]))} of
        {true, true} ->
            Dir;
        _ ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                true ->
                    ct:fail({repo_root_not_found, code:which(?MODULE)});
                false ->
                    find_repo_root(Parent)
            end
    end.

sh_quote(Value) ->
    [$' | lists:flatten(string:replace(Value, "'", "'\"'\"'", all))] ++ "'".
