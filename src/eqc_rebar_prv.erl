%% Copyright header

%% Requirement: do eqc_mocking:stop() mocking after each property has been run!
%%   Alternatively warn the user that they left chaos

-module(eqc_rebar_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, eqc).
-define(DEPS, [lock]).
-define(PLUGIN_LOCAL_EQC_DIR, "quickcheck").
%% We use lock to recompile deps (if using "compile" then default compiled with wrong flags)

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 quickcheck"}, % How to use the plugin
            {opts, [{pulse, $P, "pulse", boolean, "Compile with 'PULSE' macro and pulse_instrument parse transform"},
                    {auto_instrument, undefined, "auto_instrument", boolean, "With --pulse set to false for no additional parse_transform"},
                    %% {dir, $d, "dir", string, help(dir)},
                    {numtests, $n, "numtests", integer, "Set numtests parameter"},
                    {testing_budget, $t, "testing_budget", integer, "Set total testing time in seconds"},
                    {testing_profile, $p, "testing_profile", string, "Set the testing profile, which can determine properties to test using property_weight/2 callback"},
                    {eqc_cover, undefined, "eqc_cover", boolean, "Cover compile usign eqc_cover"},
                    {sys_config, undefined, "sys_config", string, "Path to a sys.config file to use"},
                    %%  {counterexample, $c, "counterexample", boolean, "Show counterexample"},
                    {plain, $x, "plain", boolean, "Renders plain output"},
                    {shell, $s, "shell", boolean, "Enter and Erlang shell"},
                    {compile, $c, "compile", boolean, "Only compile code, do not run quickcheck"},
                    {install, $i, "install", boolean, "Install QuickCheck before"},
                    {licence, $l, "licence", string, "Provide a site licence (be careful!)"},
                    {name, undefined, "name", atom, "Long node name for eqc node"},
                    {sname, undefined, "sname", atom, "Short node name for eqc node"}]},
            {short_desc, "Verify QuickCheck properties using eqc"},
            {desc, "Plugin to enable running eqc on QuickCheck properties. "
                   "Runs in the eqc profile and adds pulse profile if pulse option set. "
                   "Defined EQC macro and PULSE macro if pulse option set. "
                   "If you have eqc dir with files add that as extra_src_dir to eqc profile. "
                   "If you want test profile as well, run: 'rebar3 as test quickcheck'. "
                   "You cannot install and test in the same go."
            },
            {profiles, [eqc]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% Need to:
%% - find and run QuickCheck
%% - compile eqc files in ./eqc and apps/X/eqc
%% - compile source code
%%   with options like EQC and pulse parse transformsations
%% - recompile the application(s) to instrument with pulse
%% respect other profiles and commands
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Options = set_defaults(State, #{ pulse => false
                                   , auto_instrument => true %% given that pulse is specified
                                   , eqc_cover => false
                                   , sys_config => undefined
                                   , shell => false
                                   , plain => false
                                   , install => false
                                   , licence => ""
                                   , compile => false %% only compile if true
                                   }),
    %% This idea is copied from the `rebar3 shell` plugin which uses it to
    %% implement the `--config` option.
    case find_config(State) of
      undefined -> State;
      ConfigPath  ->
        case file:consult(ConfigPath) of
          {ok, Config} ->
            rebar_api:info("Loading config from ~p", [ConfigPath]),
            %% The `rebar3 shell` plugin also kills apps (with a blacklist for e.g. the kernel)
            %% before getting to this point and doing reread_config to make sure none of the apps
            %% supposed to be started by the shell have already been booted with the wrong config.
            %% It's not clear if we need to do the same thing here - but for the sake of simplicity
            %% we don't for now.
            rebar_utils:reread_config(Config, [update_logger]);
          {error, Reason} -> rebar_api:abort("Failed to load sys_config: ~p~n", [Reason])
        end
    end,
    do(State, Options).

do(State, #{install := true})->
    LatestEQC = "eqcR" ++ erlang:system_info(otp_release) ++ ".zip",
    inets:start(),
    LocalEQCDir = filename:join(filename:dirname(filename:dirname(code:which(?MODULE))),
                                ?PLUGIN_LOCAL_EQC_DIR),
    rebar_api:info("Downloading quickcheck ~s to ~p", [LatestEQC, LocalEQCDir]),
    case httpc:request("http://www.quviq.com/downloads/" ++ LatestEQC) of
        {ok, {{"HTTP/1.1", 200, "OK"}, _, Bytes}} ->
            filelib:ensure_dir(LocalEQCDir),
            case zip:extract(list_to_binary(Bytes), [memory]) of
                {ok, Files} ->
                    lists:foreach(fun({FileName, Bin}) ->
                                          Dest = filename:join([LocalEQCDir | tl(filename:split(FileName))]),
                                          ok = filelib:ensure_dir(Dest),
                                          ok = file:write_file(Dest, Bin)
                                  end, Files);
                ExtractError ->
                    rebar_api:abort("Unzip Error ~p", [ExtractError])
            end;
        Error ->
            rebar_api:abort("Download error ~p", [Error])
    end,
    {ok, State};

do(State, Options)->
    setup_name(State),

    %% Add eqc dirs to path in case they have been locally installed
    LocalEqcDir = filename:join(filename:dirname(filename:dirname(code:which(?MODULE))),
                                ?PLUGIN_LOCAL_EQC_DIR),
    case file:list_dir(LocalEqcDir) of
        {ok, Dirs} ->
            %% QuickCheck locally installed
            rebar_api:debug("Local QuickCheck in ~s found ~p", [LocalEqcDir, Dirs]),
            [ code:add_pathz(filename:join([LocalEqcDir, Dir, "ebin"])) ||
                Dir <- Dirs, filelib:is_dir(filename:join([LocalEqcDir, Dir, "ebin"])) ];
        _ ->
            rebar_api:debug("no local eqc ~s", [LocalEqcDir]),
            ok
    end,
    rebar_api:debug("Checking for eqc installation ~p", [code:get_path()]),
    check_for_eqc(State, maps:get(licence, Options)),

    %% Update erl_opts such that eqc dirs are added to source location and
    %% parse_transforms are used for all applications
    %% use rebar_app_info:update_opts to update for opts and default profile
    %% Parse transform has to be applied to all!
    State1 = def_macros(State, [{d, 'EQC'}]),
    State2 = with_pulse(State1, Options),

    Apps = rebar_state:project_apps(State),
    _ = [ begin
              rebar_api:debug("Apps ~p", [rebar_app_info:name(App)]),
              rebar_api:debug("Profiles ~p", [rebar_app_info:profiles(App)])
          end || App <- Apps ],

    %% merge ErlOpts into existing opts and default
    ErlOpts = rebar_state:get(State2, erl_opts, []),
    NewApps = [ begin
                    SrcDirs = rebar_app_info:get(App, src_dirs, ["src"]),
                    App1 = rebar_app_info:set(App, src_dirs, (SrcDirs -- ["eqc"]) ++ ["eqc"]),
                    rebar_app_info:update_opts(App1, dict:store(erl_opts, ErlOpts, dict:new()))
                end
                || App <- Apps ],
    %% Now this needs to go back in the state
    %% But we may need to add top level eqc directory to one of the apps
    code:add_pathsa([rebar_app_info:dir(App) || App <- NewApps ]),

    State3 = add_apps_and_virtual(State2, NewApps),
    State4 = load_and_compile(State3),

    do_eqc(State4, Options),
    {ok, State4}.

do_eqc(State, Options) ->
    PropDirs = rebar_state:code_paths(State, all_deps),
    rebar_api:debug("Found following directories: ~p", [ PropDirs ]),
    {EqcModules, Properties} = select_properties(PropDirs),

    case {maps:get(shell, Options), maps:get(compile, Options)} of
        {false, false} ->
            rebar_api:info("Running EQC tests...~n", []),

            case length(EqcModules) of
                0 ->
                    rebar_api:warn("No properties found: ~p", [ PropDirs ]);
                NrModules ->
                    rebar_api:info("Found following properties: ~p", [ Properties ]),

                    %% Define a testing budget if numtests not explicitly specified
                    TotalBudget = maps:get(testing_budget, Options, 20),
                    Budget = [{testing_budget, max(1, TotalBudget div NrModules)} ||
                                 maps:is_key(testing_budget, Options) orelse
                                     not maps:is_key(numtests, Options) ],
                    Numtests = [ {numtests, maps:get(numtests, Options)} ||
                                   maps:is_key(numtests, Options) ],
                    Profile  = [ {testing_profile, maps:get(testing_profile, Options)} ||
                                   maps:is_key(testing_profile, Options) ],

                    %% TODO handle skip and other results
                    EQCResults =
                        lists:foldl(fun(Mod, Acc) ->
                                          OnOutput =
                                              case maps:get(plain, Options) of
                                                  true -> [];
                                                  false -> [{on_output, fun(S, F) -> coloured_output(Mod, S, F) end}]
                                              end,
                                          Acc ++ [ {Mod, P} || P <- try eqc:module(Budget ++ Numtests ++ OnOutput ++ Profile, Mod)
                                                                    catch _:_ ->
                                                                            [{Mod, [module]}]
                                                                    end ]
                                  end, [], EqcModules),
                    case EQCResults of
                        [] ->
                            cf:print("~!gPassed ~p properties~n", [length(Properties)]);
                        Failed ->
                            cf:print("~!r~p properties, ~p failures ~!!~n", [length(Properties), length(Failed)]),
                            cf:print("~!rFailed: ~p  ~!!~n", [Failed]),
                            rebar_api:abort("Errors running QuickCheck", [])
                      end
            end;
        {true, _} ->
            rebar_prv_shell:do(State);
        {_, true} ->
            %% Only compile, let next command do something sensible with it
            ok
    end.

-spec load_and_compile(rebar_state:t()) -> rebar_state:t().
load_and_compile(State) ->
    %% Need to set path and then load all applications in deps
    %% e.g. for parse transformations or library files
    rebar_paths:set_paths([deps, plugins], State),
    [ begin
          Res = application:load(Application),
          rebar_api:debug("loading ~p -> ~p", [Application, Res])
      end || {Application, _} <- rebar_state:get(State, deps, []) ],
    %% We now have all default applications loaded
    {ok, State1} = rebar_prv_compile:do(State),
    rebar_api:debug("Done compiling", []),
    State1.

-spec add_apps_and_virtual(rebar_state:t(), [ rebar_app_info:t() ]) -> rebar_state:t().
add_apps_and_virtual(State, Apps) ->
    NewApps = Apps ++ build_root_extras(State, Apps),
    rebar_state:project_apps(State, NewApps).

build_root_extras(State, Apps) ->
    %% All extra_src_dirs are taken care of by compiler, we only add "eqc"
    BaseDir = rebar_state:dir(State),
    F = fun(App) -> rebar_app_info:dir(App) == BaseDir end,
    case lists:any(F, Apps) of
        true ->
            [];
        false ->
            %% Build an application called properties
            {ok, VApp0} = rebar_app_info:new("properties", "0.1.0", BaseDir, []),
            SrcDir = filename:join([BaseDir, "eqc"]),
            case ec_file:is_dir(SrcDir) of
                false ->
                    [];
                true ->
                    %% Check paths... we might want to write "../include" in eqc dir file
                    %% hence be on same level with rest of apps and code
                    AppFile = {application, properties,
                               [{description, "A fake OTP library"},
                                {vsn, "0.1.0"},
                                {registered, []},
                                {applications,
                                 [kernel,
                                  stdlib,
                                  eqc
                                 ]},
                                {env,[]},
                                {modules, []}
                               ]},
                    DstDir = filename:join([rebar_dir:base_dir(State), "properties"]),
                    OutDir = filename:join([DstDir, "ebin"]),
                    AppFileDir = filename:join(OutDir, "properties.app"),
                    filelib:ensure_dir(AppFileDir),
                    file:write_file(AppFileDir, io_lib:format("~p.", [AppFile])),
                    VApp1 = rebar_app_info:out_dir(VApp0, DstDir),
                    VApp2 = rebar_app_info:ebin_dir(VApp1, OutDir),
                    [rebar_app_info:set(VApp2, src_dirs, ["eqc", "test"])]
            end
    end.

coloured_output(_, ".", []) ->
    cf:print("~!g.~!!");
coloured_output(_, "Failed! "++Rest, []) ->
    cf:print("~!R~nFailed! "++Rest++"~!!");
coloured_output(_, "After ~w tests", [N]) ->
    cf:print("~!rAfter ~w tests~!!", [N]);
coloured_output(Mod, "~w: ", [P]) ->
    %% tricky, but this indicates printing the name of the property
    cf:print("~!g~p:~w: ~!!", [Mod, P]);
coloured_output(_, "~nOK, passed ~w tests~n", [N]) ->
    %% Testing budget prints newline, so no need here
    cf:print("~!gOK, passed ~w tests~!!~n", [N]);
coloured_output(_, S, F) ->
    cf:print(S, F).

-spec select_properties([ file:filename() ]) -> {[atom()], [{atom(), atom(), 0}]}.
select_properties(ProjectDirs) ->
    %% After compilation, files are already loaded
    Files =
        lists:foldl(fun(Dir, Fs) ->
                            case filelib:is_dir(Dir) of
                                true ->
                                    {ok, Items} = file:list_dir(Dir),
                                    [Item || Item <- Items,
                                             filename:extension(Item) == ".beam" ] ++ Fs;
                                false ->
                                    %% Should not happen
                                    rebar_api:warn("Cannot find files in ~p", [Dir]),
                                    Fs
                            end
                    end, [], ProjectDirs),
    Properties =
        lists:usort(
          lists:foldl(fun(BeamFile, Props) ->
                              Mod = rebar_utils:beam_to_mod(BeamFile),
                              [ {Mod, Name, 0} || {Name, 0} <- Mod:module_info(exports),
                                                  lists:prefix("prop_", atom_to_list(Name))] ++ Props
                      end, [], Files)),
    {lists:usort([M || {M,_,_} <- Properties]), Properties}.

%% Macro definitions of the form {d, Name} or {d, Name, Value}.
-spec def_macros(rebar_state:t(), [ {d, atom()} | {d, atom(), any()} ]) -> rebar_state:t().
def_macros(State, Macros) ->
    ErlOpts = rebar_state:get(State, erl_opts, []),
    NewErlOpts = (ErlOpts -- Macros) ++ Macros,
    rebar_state:set(State, erl_opts, NewErlOpts).


%% Add parse_transform, PULSE macro and pulse profile in case we run with --pulse
-spec with_pulse(rebar_state:t(), map()) -> rebar_state:t().
with_pulse(State, #{pulse := true, auto_instrument := Instrument}) ->
    Profiles = rebar_state:current_profiles(State),
    State1 =
        case lists:member(pulse, Profiles)  of
            true ->
                State;
            false ->
                rebar_api:info("Adding pulse profile", []),
                rebar_state:current_profiles(State, Profiles ++ [pulse])
        end,
    rebar_api:info("Compiling for pulse", []),
    ErlOpts = rebar_state:get(State1, erl_opts, []),
    NewErlOpts =
        ErlOpts ++
        [{d, 'PULSE'} || not lists:member({d, 'PULSE'}, ErlOpts)] ++
        [{parse_transform, pulse_instrument} || Instrument andalso
                                                    not lists:member({parse_transform, pulse_instrument}, ErlOpts)],
    rebar_state:set(State1, erl_opts, NewErlOpts);
with_pulse(State, #{pulse := false}) ->
    case lists:member(pulse, rebar_state:current_profiles(State)) of
        true ->
            rebar_api:warn("Running with profiles ~p but --pulse option not specified",
                           [rebar_state:current_profiles(State)]);
        false ->
            ok
    end,
    ErlOpts = rebar_state:get(State, erl_opts, []),
    case lists:member({d, 'PULSE'}, ErlOpts) of
        true ->
            rebar_api:warn("Macro PULSE defined but --pulse option not specified");
        false ->
            ok
    end,
     case lists:member({parse_transform, pulse_instrument}, ErlOpts) of
        true ->
            rebar_api:warn("erl_opts contains pulse_instrument, "
                           "but --pulse option not specified: ~p", [ErlOpts]);
        false ->
            ok
    end,
    State.

check_for_eqc(State, Licence) ->
    DefaultPaths = rebar_state:code_paths(State, default),
    Libs = [ case lists:reverse(filename:split(X)) of
                 ["ebin", Lib | _ ] -> Lib;
                 _ -> ""
             end || X <- DefaultPaths ],
    EqcLibs = lists:filter(fun("eqc-"++_) -> true;
                              ("pulse-"++_) -> true;
                              (_) -> false
                           end, Libs),
    EqcPath = code:which(eqc),
    case {EqcPath, EqcLibs} of
        {non_existing, []} ->
            rebar_api:abort("No eqc installation available", []);
        {non_existing, _} ->
            rebar_api:abort("Eqc in path ~p, but not available?", [EqcLibs]);
        {_, []} ->
            %%  This happens when eqc is installed in plugin
            rebar_api:warn("Eqc used in path ~p", [EqcPath]);
        _ ->
            ok
    end,

    %% Now QuickCheck should work
    case string:length(Licence) > 0 of
        false ->
            ok;
        true ->
            rebar_api:debug("Forcing licence registration\n", []),
            eqc:force_registration([Licence])
    end,
    %% Start quickcheck to update licence check
    case eqc:start() of
        ok ->
            case eqc:version() < 1.44 of
                true ->
                    %% Some things might work, such as running a compiling and running a shell
                    rebar_api:error("QuickCheck version 1.44.1 or later required",  []);
                false ->
                    ok
            end;
        Error ->
            rebar_api:abort("Cannot start QuickCheck ~p", [Error])
    end.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%% Defaults if not provided
%% ConfigOptions (provided in rebar.config) overwrite defaults
%% Args (command line) overwrite ConfigOptions
-spec set_defaults(rebar_state:t(), map()) -> map().
set_defaults(State, Defaults) ->
    ConfigOptions = maps:from_list(rebar_state:get(State, ?PROVIDER, [])),
    {Args, _} = rebar_state:command_parsed_args(State),
    ArgOptions = maps:from_list(Args),
    maps:merge(Defaults, maps:merge(ConfigOptions, ArgOptions)).

setup_name(State) ->
    {Long, Short, Opts} = rebar_dist_utils:find_options(State),
    rebar_dist_utils:either(Long, Short, Opts).

%% Command line overrides sys_config in rebar.config
find_config(State) ->
  {Opts, _} = rebar_state:command_parsed_args(State),
  case proplists:get_value(sys_config, Opts, undefined) of
    undefined -> rebar_state:get(State, sys_config, undefined);
    FilePath  -> FilePath
  end.
