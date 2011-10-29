# Rebar alt_deps Plugin

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

- Manage dependencies from a single (local) repository
- Fetch dependencies from various *scm* repositories, a la `rebar_deps`
- Fetch artefacts from [nexus](http://nexus.sonatype.org/) repositories
- Fetch artefacts from [erlware](http://erlware.org) repositories

## Running the integration tests

You can run the integration tests by issuing the following command:

    $ ./rebar -C test.config get-deps compile retest -v
