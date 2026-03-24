-module(eqc_rebar_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

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
    testing_budget_scales_work/1,
    testing_profile_filters_properties/1,
    testing_profile_budget_stays_per_module/1,
    eqc_cover_writes_outputs/1,
    eqc_cover_none_suppresses_outputs/1
]).

all() ->
    [help_eqc_from_snapshot,
     eqc_passes_from_snapshot,
     eqc_fails_from_snapshot,
     as_test_eqc_from_snapshot,
     plain_output_from_snapshot,
     top_level_eqc_from_snapshot,
     testing_budget_covers_all_modules,
     testing_budget_scales_work,
     testing_profile_filters_properties,
     testing_profile_budget_stays_per_module,
     eqc_cover_writes_outputs,
     eqc_cover_none_suppresses_outputs].

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
    CleanOutput = strip_ansi(Output),
    case {Status =/= 0,
          contains(CleanOutput, "sample_fail_eqc:prop_broken:"),
          contains(CleanOutput, "Errors running QuickCheck")} of
        {true, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_eqc_failure_output, Status, CleanOutput})
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
    PropertyCounts = extract_property_pass_counts(CleanOutput),
    ModuleTotals = module_totals(PropertyCounts),
    case {Status,
          maps:get("sample_budget_one_eqc", ModuleTotals, 0) > 0,
          maps:get("sample_budget_two_eqc", ModuleTotals, 0) > 0} of
        {0, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_testing_budget_output, Status, PropertyCounts, CleanOutput})
    end.

testing_budget_scales_work(Config) ->
    FixtureDir = prepare_fixture(Config, "budget_property"),
    #{status := LowStatus, output := LowOutput} = run_shell(FixtureDir, "rebar3 eqc --testing_budget 2"),
    #{status := HighStatus, output := HighOutput} = run_shell(FixtureDir, "rebar3 eqc --testing_budget 4"),
    LowCounts = extract_pass_counts(strip_ansi(LowOutput)),
    HighCounts = extract_pass_counts(strip_ansi(HighOutput)),
    case {LowStatus, HighStatus,
          length(LowCounts) > 0, length(HighCounts) > 0,
          lists:sum(HighCounts) > lists:sum(LowCounts)} of
        {0, 0, true, true, true} ->
            ok;
        _ ->
            ct:fail({testing_budget_did_not_scale, LowCounts, HighCounts, LowOutput, HighOutput})
    end.

testing_profile_filters_properties(Config) ->
    FixtureDir = prepare_fixture(Config, "failing_property"),
    #{status := Status, output := Output} =
        run_shell(FixtureDir, "rebar3 eqc --numtests 3 --testing_profile focused"),
    CleanOutput = strip_ansi(Output),
    case {Status,
          contains(CleanOutput, "sample_fail_eqc:prop_safe"),
          not contains(CleanOutput, "sample_fail_eqc:prop_broken:"),
          not contains(CleanOutput, "Errors running QuickCheck")} of
        {0, true, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_testing_profile_output, Status, CleanOutput})
    end.

testing_profile_budget_stays_per_module(Config) ->
    FixtureDir = prepare_fixture(Config, "budget_property"),
    #{status := Status, output := Output} =
        run_shell(FixtureDir, "rebar3 eqc --testing_budget 4 --testing_profile focused"),
    CleanOutput = strip_ansi(Output),
    PropertyCounts = maps:from_list(extract_property_pass_counts(CleanOutput)),
    OneCount = maps:get({"sample_budget_one_eqc", "prop_budget_one_selected"}, PropertyCounts, 0),
    TwoCount = maps:get({"sample_budget_two_eqc", "prop_budget_two_selected"}, PropertyCounts, 0),
    case {Status,
          OneCount > 0,
          TwoCount > 0,
          not contains(CleanOutput, "prop_budget_one_hidden:"),
          not contains(CleanOutput, "prop_budget_two_hidden:"),
          abs(OneCount - TwoCount) =< 2} of
        {0, true, true, true, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_budget_profile_output, Status, PropertyCounts, CleanOutput})
    end.

eqc_cover_writes_outputs(Config) ->
    FixtureDir = prepare_fixture(Config, "passing_property"),
    #{status := Status, output := Output} =
        run_shell(FixtureDir,
                  "rebar3 eqc --numtests 4 --eqc_cover --eqc_cover_ticks cover.ticks "
                  "--eqc_cover_html cover-html"),
    CleanOutput = strip_ansi(Output),
    TicksFile = filename:join(FixtureDir, "cover.ticks"),
    HtmlIndex = filename:join([FixtureDir, "cover-html", "index.html"]),
    SourceHtml = filename:join([FixtureDir, "cover-html", "sample_pass.erl.html"]),
    Ticks = read_cover_ticks(TicksFile),
    SourceHtmlContent = read_text_file(SourceHtml),
    case {Status,
          contains(CleanOutput, "Coverage of "),
          contains(CleanOutput, "sample_pass.erl: 50.0%"),
          filelib:is_file(TicksFile),
          filelib:is_file(HtmlIndex),
          file_size(TicksFile) > 0,
          contains(SourceHtmlContent, "class=red"),
          contains(SourceHtmlContent, "uncovered"),
          cover_ticks_contains_module(Ticks, sample_pass),
          cover_ticks_contains_label(Ticks, "sample_pass_eqc:prop_reverse_reverse")} of
        {0, true, true, true, true, true, true, true, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_eqc_cover_output, Status, CleanOutput, Ticks, SourceHtmlContent})
    end.

eqc_cover_none_suppresses_outputs(Config) ->
    FixtureDir = prepare_fixture(Config, "passing_property"),
    #{status := Status, output := Output} =
        run_shell(FixtureDir,
                  "rebar3 eqc --numtests 4 --eqc_cover --eqc_cover_ticks none "
                  "--eqc_cover_html none"),
    CleanOutput = strip_ansi(Output),
    TicksFile = filename:join(FixtureDir, "cover.ticks"),
    HtmlDir = filename:join(FixtureDir, "cover-html"),
    case {Status,
          not contains(CleanOutput, "Coverage of "),
          not filelib:is_file(TicksFile),
          not filelib:is_dir(HtmlDir)} of
        {0, true, true, true} ->
            ok;
        _ ->
            ct:fail({unexpected_eqc_cover_none_output, Status, CleanOutput, TicksFile, HtmlDir})
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

extract_property_pass_counts(Text) ->
    Lines = string:split(Text, "\n", all),
    extract_property_pass_counts(Lines, undefined, []).

extract_property_pass_counts([], _Current, Acc) ->
    lists:reverse(Acc);
extract_property_pass_counts([Line | Rest], Current, Acc) ->
    case re:run(Line, "^([[:alnum:]_]+):([[:alnum:]_]+):", [{capture, all_but_first, list}]) of
        {match, [Module, Prop]} ->
            extract_property_pass_counts(Rest, {Module, Prop}, Acc);
        nomatch ->
            case {Current, re:run(Line, "^OK, passed ([0-9]+) tests$", [{capture, all_but_first, list}])} of
                {{Module, Prop}, {match, [Count]}} ->
                    extract_property_pass_counts(Rest, undefined,
                                                 [{{Module, Prop}, list_to_integer(Count)} | Acc]);
                _ ->
                    extract_property_pass_counts(Rest, Current, Acc)
            end
    end.

module_totals(PropertyCounts) ->
    lists:foldl(fun({{Module, _Prop}, Count}, Acc) ->
                        maps:update_with(Module, fun(Total) -> Total + Count end, Count, Acc)
                end, #{}, PropertyCounts).

file_size(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{size = Size}} ->
            Size;
        _ ->
            0
    end.

read_cover_ticks(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            erlang:binary_to_term(Bin);
        Error ->
            Error
    end.

read_text_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            unicode:characters_to_list(Bin);
        _ ->
            ""
    end.

cover_ticks_contains_module(Ticks, Module) when is_list(Ticks) ->
    lists:any(fun({TickModule, _}) -> TickModule =:= Module;
                 (_) -> false
              end, Ticks);
cover_ticks_contains_module(_, _) ->
    false.

cover_ticks_contains_label(Ticks, Label) when is_list(Ticks) ->
    lists:any(fun({_Module, Entries}) ->
                      entries_contain_label(Entries, Label)
              end, Ticks);
cover_ticks_contains_label(_, _) ->
    false.

entries_contain_label(Entries, Label) when is_list(Entries) ->
    lists:any(fun({_Line, Calls}) ->
                      calls_contain_label(Calls, Label)
              end, Entries);
entries_contain_label(_, _) ->
    false.

calls_contain_label(Calls, Label) when is_list(Calls) ->
    lists:any(fun({CallLabel, _Count}) -> CallLabel =:= Label;
                 (_) -> false
              end, Calls);
calls_contain_label(_, _) ->
    false.

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
