%% Copyright header

%% Requirement: do eqc_mocking:stop() mocking after each property has been run!
%%   Alternatively warn the user that they left chaos

-module(eqc_rebar_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, eqc).
-define(DEPS, [lock]).
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
                    {properties, $p, "properties", string, "Names of properties to check"},
                    {eqc_cover, undefined, "eqc_cover", boolean, "Cover compile usign eqc_cover"},
                    %%  {counterexample, $c, "counterexample", boolean, "Show counterexample"},
                    {plain, $x, "plain", boolean, "Renders plain output"},
                    {shell, $s, "shell", boolean, "Enter and Erlang shell"},
                    {compile, $c, "compile", boolean, "Only compile code, do not run quickcheck"},
                    {name, undefined, "name", atom, "Long node name for eqc node"},
                    {sname, undefined, "sname", atom, "Short node name for eqc node"}]},
            {short_desc, "Verify QuickCheck properties using eqc"},
            {desc, "Plugin to enable running eqc on QuickCheck properties. "
                   "Runs in the eqc profile and adds pulse profile if pulse option set. "
                   "Defined EQC macro and PULSE macro if pulse option set. "
                   "If you have eqc dir with files add that as extra_src_dir to eqc profile. "
                   "If you want test profile as well, run: 'rebar3 as test quickcheck'. "
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
                                   , shell => false
                                   , plain => false
                                   , compile => false %% only compile if true
                                   }),

    setup_name(State),
    check_for_eqc(State),

    %% Update erl_opts such that eqc dirs are added to source location and
    %% parse_transforms are used for all applications
    %% use rebar_app_info:update_opts to update for opts and default profile
    %% Parse transform has to be applied to all!
    State1 = def_macros(State, [{d, 'EQC'}]),
    State2 = add_src_dirs(State1, ["eqc", "src"]),
    State3 = with_pulse(State2, Options),

    Apps = rebar_state:project_apps(State),
    _ = [ begin
              rebar_api:info("Apps ~p", [rebar_app_info:name(App)]),
              rebar_api:info("Profiles ~p", [rebar_app_info:profiles(App)])
          end || App <- Apps ],

    %% merge ErlOpts into existing opts and default
    ErlOpts = rebar_state:get(State3, erl_opts, []),
    NewApps = [ rebar_app_info:update_opts(App, dict:store(erl_opts, ErlOpts, dict:new()))
                || App <- Apps ],
    %% Now this needs to go back in the state
    %% But we may need to add top level eqc directory to one of the apps
    State4 = add_apps_and_virtual(State3, NewApps),

    %% TODO Explore rebar_prv_compile:build_root_extras(State, Apps)
    %% check how eunit adds top level test directory

    %% To see if one can create an extras dir without including it in lib include path
    %% overwriting eqc include path
    %% rebar_api:info("NewApps ~p", [NewApps]),


    State5 = load_and_compile(State4),

    do_eqc(State5, Options),
    {ok, State5}.

do_eqc(State, Options) ->
    PropDirs = [ rebar_app_info:ebin_dir(App) || App <- rebar_state:project_apps(State) ],
    rebar_api:debug("Found following directories: ~p", [ PropDirs ]),

    rebar_api:info("Found following directories: ~p", [ PropDirs ]),
    {EqcModules, Properties} = select_properties(PropDirs, Options),

    %% rebar_api:info("State ~p", [ State ]),
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

                    %% TODO handle skip and other results
                    EQCResults =
                        lists:foldl(fun(Mod, Acc) ->
                                          OnOutput =
                                              case maps:get(plain, Options) of
                                                  true -> [];
                                                  false -> [{on_output, fun(S, F) -> coloured_output(Mod, S, F) end}]
                                              end,
                                          Acc ++ [ {Mod, P} || P <- try eqc:module(Budget ++ Numtests ++ OnOutput, Mod)
                                                                    catch _:_ ->
                                                                            [{Mod, [module]}]
                                                                    end ]
                                  end, [], EqcModules),
                    case EQCResults of
                        [] ->
                            cf:print("~!gPassed ~p properties~n", [length(Properties)]);
                        Failed ->
                            cf:print("~!r~p properties, ~p failures ~!!~n", [length(Properties), length(Failed)]),
                            rebar_api:error("Errors running QuickCheck", [])
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
            %% TODO see if we can use plugin dir eqc_rebar without name clash
            _ProjOpts = rebar_state:opts(State),
            %% rebar_api:info("Creating top level app ~p with basedir ~p", [ProjOpts, BaseDir]),
            {ok, VApp0} = rebar_app_info:new("properties", "0.1.0", BaseDir, []),
            SrcDir = filename:join([BaseDir, "eqc"]),
            case ec_file:is_dir(SrcDir) of
                false ->
                    [];
                true ->
                    OutDir = filename:join([BaseDir, "_build", "eqc", "lib", "properties"]),
                    VApp1 = rebar_app_info:out_dir(VApp0, OutDir),
                    VApp2 = rebar_app_info:ebin_dir(VApp1, filename:join(OutDir, "ebin")),
                    Opts = rebar_state:opts(State),
                    VApp3 = rebar_app_info:opts(VApp2, Opts),
                    VApp4 = rebar_app_info:set(VApp3, src_dirs, ["eqc"]),
                    rebar_api:info("App = ~p", [VApp4]),
                    [VApp4]
            end
    end.

coloured_output(_, ".", []) ->
    cf:print("~!g.");
coloured_output(_, "x", []) ->
    cf:print("~!y*");
coloured_output(_, "Failed! ", []) ->
    cf:print("~!r~nFailed! ~!!");
coloured_output(_, "After ~w tests", [N]) ->
    cf:print("~!rAfter ~w tests ", [N]);
coloured_output(Mod, "~w: ", [P]) ->
    %% tricky, but this indicates printing the name of the property
    cf:print("~!g~p:~w: ", [Mod, P]);
coloured_output(_, "~nOK, passed ~w tests~n", [N]) ->
    %% Testing budget prints newline, so no need here
    cf:print("~!gOK, passed ~w tests~n", [N]);
coloured_output(_, S, F) ->
    io:format(S, F).

-spec select_properties([ file:filename() ], map()) -> {[atom()], [{atom(), atom(), 0}]}.
select_properties(ProjectDirs, _Options) ->
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
        lists:foldl(fun(BeamFile, Props) ->
                            Mod = rebar_utils:beam_to_mod(BeamFile),
                            [ {Mod, Name, 0} || {Name, 0} <- Mod:module_info(exports),
                                                lists:prefix("prop_", atom_to_list(Name)) ] ++ Props
                    end, [], Files),
    {lists:usort([M || {M,_,_} <- Properties]), Properties}.

%% When running in test profile, the "test" directory is added as an extra_src_dir
%% That is essential in order not to have beam copies in basedir/test
%% It is a mystery why test files are occuring as beam in both _build/../ebin and _build/../test
-spec add_src_dirs(rebar_state:t(), [ file:filename() ]) -> rebar_state:t().
add_src_dirs(State, Dirs) ->
    SrcDirs = rebar_dir:src_dirs(rebar_state:opts(State)),
    case lists:member("test", Dirs) of
        true ->
            rebar_api:warn("Found \"test\" in src_dirs ~p, "
                           "rather run with \"as test\" profile", [Dirs]);
        false ->
            rebar_api:debug("Identified src_dirs ~p", [SrcDirs])
    end,
    ErlOpts = rebar_state:get(State, erl_opts, []),
    NewSrcDirs = (SrcDirs -- Dirs) ++ Dirs,
    NewErlOpts = [{src_dirs, NewSrcDirs} | proplists:delete(src_dirs, ErlOpts)],
    rebar_state:set(State, erl_opts, NewErlOpts).

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

check_for_eqc(State) ->
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
            rebar_api:error("No eqc installation available");
        {non_existing, _} ->
            rebar_api:error("Eqc in path ~p, but not available?", [EqcLibs]);
        {_, []} ->
            rebar_api:warn("Eqc available but not in rebar3 code path", [EqcPath]);
        _ ->
            ok
    end,
    %% Start quickcheck to update licence check
    eqc:start().


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