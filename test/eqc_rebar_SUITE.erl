-module(eqc_rebar_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    help_eqc_from_snapshot/1,
    eqc_passes_from_snapshot/1,
    eqc_fails_from_snapshot/1,
    as_test_eqc_from_snapshot/1,
    plain_output_from_snapshot/1,
    top_level_eqc_from_snapshot/1,
    testing_budget_covers_all_modules/1,
    testing_budget_scales_work/1
]).

all() ->
    [help_eqc_from_snapshot,
     eqc_passes_from_snapshot,
     eqc_fails_from_snapshot,
     as_test_eqc_from_snapshot,
     plain_output_from_snapshot,
     top_level_eqc_from_snapshot,
     testing_budget_covers_all_modules,
     testing_budget_scales_work].

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
    FixtureDir = prepare_fixture(Config, "help_plugin"),
    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 help eqc"),
    case {Status, contains(Output, "Usage: rebar3 eqc"),
                  contains(Output, "Plugin to enable running eqc on QuickCheck properties")} of
        {0, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_rebar3_output, Status, Output})
    end.

eqc_passes_from_snapshot(Config) ->
    FixtureDir = prepare_fixture(Config, "passing_property"),
    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 eqc --numtests 5"),
    case {Status, contains(Output, "Passed 1 properties"),
                  contains(Output, "sample_pass_eqc:prop_reverse_reverse")} of
        {0, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_eqc_success_output, Status, Output})
    end.

eqc_fails_from_snapshot(Config) ->
    FixtureDir = prepare_fixture(Config, "failing_property"),
    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 eqc --numtests 5"),
    case {Status =/= 0, contains(Output, "1 properties, 1 failures"),
                        contains(Output, "Errors running QuickCheck")} of
        {true, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_eqc_failure_output, Status, Output})
    end.

as_test_eqc_from_snapshot(Config) ->
    FixtureDir = prepare_fixture(Config, "passing_property"),
    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 as test eqc --numtests 3"),
    case {Status, contains(Output, "Passed 2 properties"),
                  contains(Output, "sample_pass_eqc:prop_reverse_reverse"),
                  contains(Output, "sample_pass_in_test_eqc:prop_test_dir_property")} of
        {0, true, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_as_test_eqc_output, Status, Output})
    end.

plain_output_from_snapshot(Config) ->
    FixtureDir = prepare_fixture(Config, "passing_property"),
    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 eqc --plain --numtests 4"),
    case {Status, contains(Output, "prop_reverse_reverse:"),
                  not contains(Output, "sample_pass_eqc:prop_reverse_reverse:")} of
        {0, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_plain_output, Status, Output})
    end.

top_level_eqc_from_snapshot(Config) ->
    FixtureDir = prepare_fixture(Config, "top_level_property"),
    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 eqc --numtests 4"),
    case {Status, contains(Output, "Passed 1 properties"),
                  contains(Output, "sample_app_eqc:prop_top_level")} of
        {0, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_top_level_eqc_output, Status, Output})
    end.

testing_budget_covers_all_modules(Config) ->
    FixtureDir = prepare_fixture(Config, "budget_property"),
    #{status := Status, output := Output} = run_shell(FixtureDir, "rebar3 eqc --testing_budget 2"),
    CleanOutput = strip_ansi(Output),
    Counts = extract_pass_counts(CleanOutput),
    case {Status,
          contains(CleanOutput, "sample_budget_one_eqc:prop_budget_one"),
          contains(CleanOutput, "sample_budget_two_eqc:prop_budget_two"),
          length(Counts),
          lists:all(fun(Count) -> Count > 0 end, Counts)} of
        {0, true, true, 2, true} ->
            ok;
        _ ->
            ct:fail({unexpected_testing_budget_output, Status, Counts, CleanOutput})
    end.

testing_budget_scales_work(Config) ->
    FixtureDir = prepare_fixture(Config, "budget_property"),
    #{status := LowStatus, output := LowOutput} = run_shell(FixtureDir, "rebar3 eqc --testing_budget 2"),
    #{status := HighStatus, output := HighOutput} = run_shell(FixtureDir, "rebar3 eqc --testing_budget 4"),
    LowCounts = extract_pass_counts(strip_ansi(LowOutput)),
    HighCounts = extract_pass_counts(strip_ansi(HighOutput)),
    case {LowStatus, HighStatus, length(LowCounts), length(HighCounts),
          lists:sum(HighCounts) > lists:sum(LowCounts)} of
        {0, 0, 2, 2, true} ->
            ok;
        _ ->
            ct:fail({testing_budget_did_not_scale, LowCounts, HighCounts, LowOutput, HighOutput})
    end.

make_temp_dir(Prefix) ->
    Base = case os:getenv("TMPDIR") of
               false -> "/tmp";
               TmpDir -> TmpDir
           end,
    Dir = filename:join(Base, Prefix ++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    Dir.

prepare_fixture(Config, FixtureName) ->
    RepoRoot = ?config(repo_root, Config),
    TmpRoot = ?config(tmp_root, Config),
    SnapshotDir = filename:join(TmpRoot, "plugin"),
    FixtureTemplateDir = filename:join([RepoRoot, "fixtures", FixtureName]),
    FixtureDir = filename:join(TmpRoot, FixtureName),
    ok = snapshot_repo(RepoRoot, SnapshotDir),
    ok = copy_tree(FixtureTemplateDir, FixtureDir),
    ok = inject_plugin_url(filename:join(FixtureDir, "rebar.config"),
                           "file://" ++ SnapshotDir),
    FixtureDir.

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

strip_ansi(Text) ->
    re:replace(Text, "\e\\[[0-9;]*m", "", [global, {return, list}]).

extract_pass_counts(Text) ->
    Matches = re:run(Text, "OK, passed ([0-9]+) tests", [global, {capture, all_but_first, list}]),
    case Matches of
        {match, Captures} ->
            [list_to_integer(Count) || [Count] <- Captures];
        nomatch ->
            []
    end.

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
