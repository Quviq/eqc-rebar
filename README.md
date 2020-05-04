Quviq's eqc-rebar
=====

A rebar plugin for using Quviq's Erlang QuickCheck (eqc) in rebar3
projects.

This plugin is a re-design of earlier useful plugins for compiling and
running eqc properties:
- https://github.com/Vagabond/rebar3-eqc-plugin
- https://github.com/kellymclaughlin/rebar3-eqc-plugin


Use
---

Add the plugin to your rebar config:

    {plugins, [
        {eqc_rebar, {git, "https://github.com/Quviq/eqc-rebar.git", {tag, "1.0.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 eqc
    ===> Fetching eqc_rebar
    ===> Compiling eqc_rebar
    <Plugin Output>


EQC environment
====

The plugin will compile files in `src` and `eqc` directory (decending
into apps directories). During compilation, the `EQC` macro is defined, in
other words, the erlang compiler option `{d, EQC}` is automatically
specified.

If an `eqc` profile is defined in the rebar.config file, then
additional settings in this profile will be used.

After compilation, all QuickCheck properties found are checked with
Quviq's quickcheck. The total testing time is by default 20 seconds
and can be adjusted by the `testing_budget` option. The budget is
devided equally over the number of modules provided.


Directory structure
----

We recommend to put QuickCheck properties in the eqc directory. In
that way one does not interfere with any other testing tool. These
properties will be picked up by the plugin and checked.

If one would like to verify eunit tests, common test tests and eqc
properties, then running these in sequence is perfectly valid:
```
rebar3 do eunit, eqc --testing_budget 120, ct
```
Look in the rebar3 documentation to see how aliases can be created to
support this in one command.

(todo: getting combined coverage)

The test profile is not selectedf by default. That means that the
macro `TEST` is not defined by the eqc_rebar plugin, neither are
files in the `test` directory compiled.

Properties in `test`directory
---

One can add the test profile to combine tests and quickcheck. In this
case the active profile will be `test+eqc` in which files in both the
`test` and the `eqc` directory are compiled with the macros `TEST` and
`EQC` both defined.
```shell
rebar3 as test eqc
```
Note that this command checks the properties defined in both test and eqc
directories, but it does not run the unit tests. In this context
`eqc` is the command not the profile.

As explained above, verifying both the properties as well as the unit
tests would best be obtained by:
```shell
rebar3 do eqc eunit
```
and if one has properties in the `test` directory then one would run it with a profile:
```shell
rebar3  as test do eqc
```
This is subtly different from `rebar3 as test do eunit, eqc` because
this latter runs eunit without compiling files in eqc directory and without `EQC` macro defined.
That means that properties in test directory
excluded with `-ifdef(EQC)` are not part of the eunit tests. Moreover,
it would copy beam files in the top level test diretory, which
certainly is not what you want.



Properties in `src` directory
---

Properties in the source code are autiomatcially detected by
the plugin and checked by QuickCheck. It is not advised to add
properties to source code, this creates a dependency between a
distribution and quickcheck, which would be totally unnecessary.

Integration testing
===

In the case of several Apps in a repository, using the `apps/App/*`
directory structure, one often wants to define integration tests for
the combination of these Apps. These tests are typically placed in the
root level in `test`or, in the case of this plugin, in the top level
`eqc` directory for properties. By not defining the macro `TEST` by
default, the QuickCheck properties can be verified for
the actual released software that should not export functions that one
may have `-ifdef`-ed using the `TEST` macro.

Working in Erlang shell
====

One can run a shell with quickcheck compiled code by providing the
`shell` option.
```shell
rebar3 eqc --shell
```
This differs from `rebar3 do eqc, shell` by the fact that the latter
first checks all properties and then provides an Erlang shell.

EQC options
---

--pulse


TODO
---

- apps/App/src projects does not add apps in library when compiling
  toplevel eqc
- rebar3 as test eqc seems to create .beam files in test directory,
most likely need to add  test to src_dirs when test profile chosen
- rebar3 do eqc, eunit versus rebar3 do eqc eunit
- distribution: --name, --sname
- explain all options and get pulse to run
