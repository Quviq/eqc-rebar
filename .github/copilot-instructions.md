# eqc-rebar repository instructions

## Build and test commands

- Build the plugin: `rebar3 compile`
- Run the Common Test harness: `rebar3 ct`
- Run a single Common Test case: `rebar3 ct --suite test/eqc_rebar_SUITE --case <testcase>`
- Run EUnit if you are touching any EUnit-facing behavior: `rebar3 eunit`

There is no lint target configured in `rebar.config`.

For downstream usage examples of the plugin itself, the README is the source of truth:

- `rebar3 eqc`
- `rebar3 as test eqc`
- `rebar3 as eqc eunit`
- `rebar3 eqc --shell`

## High-level architecture

- `src/eqc_rebar.erl` is only the plugin entrypoint. It delegates provider registration to `eqc_rebar_prv:init/1`.
- `src/eqc_rebar_prv.erl` contains essentially all behavior. It registers the `eqc` provider, defines CLI options, and runs the compile/discovery/execution pipeline.
- `test/eqc_rebar_SUITE.erl` is the repository's end-to-end test harness. It snapshots the current working tree into a temporary local git repo, injects that snapshot into fixture projects under `fixtures/`, and drives real `rebar3` commands.
- Provider option precedence is implemented in `set_defaults/2`: built-in defaults < `rebar.config` `eqc` options < command-line flags.
- The provider can optionally load a `sys.config` before running. CLI `--sys_config` overrides any configured value.
- Before compiling, the provider:
  - verifies QuickCheck is available, optionally forcing licence registration
  - always defines the `EQC` macro
  - conditionally enables `PULSE` and `pulse_instrument`
  - conditionally prepends the `eqc_cover` parse transform
  - adds `eqc` to each app's `src_dirs`
- If the repository has a top-level `eqc/` directory and no root application, `build_root_extras/2` synthesizes a virtual application named `properties` so root-level properties can still compile.
- Compilation is delegated back to `rebar_prv_compile:do/1`; property discovery happens after compilation by scanning compiled `.beam` exports, not by parsing source files.
- A function is treated as a QuickCheck property only if it is an exported zero-arity function whose name starts with `prop_`. `select_properties/1` is the discovery point to update if that rule changes.
- Running the provider normally calls `eqc:module/2` for each discovered property module. The testing budget is split evenly per module unless `--numtests` is explicitly set.
- `--shell` skips property execution and hands off to `rebar_prv_shell:do/1`. `--compile` stops after compilation.

## Key conventions

- Keep behavioral changes centered in `src/eqc_rebar_prv.erl`. Most repository changes should be small edits to provider setup, state mutation, or post-compile property selection rather than spread across new modules.
- Prefer expanding the existing Common Test harness over adding ad hoc scripts. The current harness already covers provider loading, success/failure runs, `as test eqc`, `testing_budget`, `testing_profile`, top-level `eqc/`, and `eqc_cover`.
- Preserve the distinction between `eqc` and `test` code paths from the README and provider implementation:
  - `EQC` is defined automatically when this provider runs.
  - `TEST` is not defined by default.
  - `rebar3 as test eqc` compiles both `test` and `eqc` directories, but it does not run EUnit tests.
- Keep fixture projects under `fixtures/`, not under `test/`. Putting toy OTP apps under `test/` makes `rebar3 eunit` try to discover them as project test modules.
- Follow the repository's property naming convention in fixtures and examples: a module like `foo` is paired with a property module like `foo_eqc`.
- When changing property discovery, remember that this plugin works from compiled beam exports. Unexported `prop_*` functions are invisible to the runner.
- When changing compile-time behavior, update both provider state and app metadata consistently. The current flow updates global `erl_opts`, then updates each app's `src_dirs` and per-app opts before recompiling.
- `--pulse` is intentionally stateful: it appends the `pulse` profile if missing and injects both the `PULSE` macro and `pulse_instrument` parse transform unless `--auto_instrument false` is used.
- When using `--install` QuickCheck is stored under `quickcheck/` next to the plugin code, and the provider adds any discovered `ebin` directories from there to the code path before checking availability.
