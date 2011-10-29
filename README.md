# Rebar alt_deps Plugin

This plugin provides an alternative mechanism for working with deps, which can
be combined with the 
[rebar_alien_plugin](http://github.com/hyperthunk/rebar_alien_plugin) to manage
dependencies which are not compatible with rebar (for whatever reason).

Currently the only type of dependencies that are supported are explicit tags
for repositories hosted in either *github* or *bitbucket*.

## Notices/Caveats

This plugin relies on a feature which is currently sitting in the official 
rebar repository as a pull request. Until this is merged, you will need to use
[this fork of rebar](https://github.com/hyperthunk/rebar/tree/pub-cmd-alt-deps)
in order to utilise the plugin. There is no guarantee that the pull request will
be accepted, so this plugin will remain *experimental* until such time as it is.

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
