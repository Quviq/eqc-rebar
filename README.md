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

Add the plugin to your rebar configof an existing application:

    {plugins, [
        {eqc_rebar, {git, "https://github.com/Quviq/eqc-rebar.git", {branch, "master"}}}
    ]}.

Create an `eqc` directory in the same way as you have a `src` and
`test` directory and keep you properties and generators in that
`eqc` directory.

Then just call your plugin directly in this top-level application directory:

    $ rebar3 eqc
    ===> Fetching eqc_rebar
    ===> Compiling eqc_rebar
    <Plugin Output>

All properties will be extracted and checked with QuickCheck. You need
a [QuickCheck licence](mailto:support@quviq.com?eqc%20licence%20info)
for it and eqc-1.44.1 or higher installed.


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

Other users of  your repository, not having a licence, can use your
software, but cannot check the properties. The `eqc` directory can be 
ignored by them.


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

The test profile is not selected by default. That means that the
macro `TEST` is not defined by the eqc_rebar plugin, neither are
files in the `test` directory compiled.

Properties in `test` directory
---

One can add the test profile to combine tests and quickcheck. In this
case the active profile will be `test+eqc` in which files in both the
`test` and the `eqc` directory are compiled with the macros `TEST` and
`EQC` both defined.
```bash
rebar3 as test eqc
```
Note that this command checks the properties defined in both test and eqc
directories, but it does not run the unit tests. In this context
`eqc` is the command not the profile.

Unit tests should be run with `rebar3 eunit` and if you have added quickcheck 
tests in the `test` directory that are called from within a test:
```erlang
many_test() ->
  eqc:quickcheck(prop_many()).
```
then one normally would us `-ifdef(EQC) ... -endif.` it to exclude these
tests for people not having a licence.
Add `{d, 'EQC'}` to the erl_opts in an eqc profile in rebar.config to make it 
work with eunit.
```bash
rebar3 as eqc eunit
```
 
Properties in `src` directory
---

Properties in the source code are automatically detected by
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
```bash
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
