# Rebar Alternative Dependencies Plugin

This plugin provides an alternative mechanism for working with deps, which can
be combined with the
[rebar_alien_plugin](http://github.com/hyperthunk/rebar_alien_plugin) to manage
dependencies which are not compatible with rebar (for whatever reason).

Currently the only type of dependencies that are supported are explicit tags
for repositories hosted in either *github* or *bitbucket*.

## Sample usage

See the `examples` folder for some of the use-cases.

    $ rebar install-deps -v

## Roadmap

- Support overriding the build configuration when compiling dependencies
- Manage both dependencies *and* plugins without requiring extra build steps
- Manage dependencies from a single (local) repository
- Fetch dependencies from various *scm* repositories, a la `rebar_deps`
- Fetch artefacts from [nexus](http://nexus.sonatype.org/) repositories
- Fetch artefacts from [erlware](http://erlware.org) repositories

## Running the integration tests

In order to run the integration tests, you will need a copy of the rebar binary
which supports https://github.com/basho/rebar/pull/156, which currently means you
will need to get hold of
[this fork](https://github.com/hyperthunk/rebar/tree/merge-config). A copy of this
binary is included in the repository, which has been compiled against R14B01.

You can run the integration tests by issuing the following command:

    $ ./rebar get-deps compile
    $ ./rebar -C test.config get-deps compile retest -v
