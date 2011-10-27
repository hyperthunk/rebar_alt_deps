# Rebar alt_deps Plugin

This plugin provides an alternative mechanism for working with deps, which can
be combined with the 
[rebar_alien_plugin](http://github.com/hyperthunk/rebar_alien_plugin) to manage
dependencies which are not compatible with rebar (for whatever reason).

## Sample usage

See the `examples` folder for some of the use-cases.

    $ rebar install-deps -v

## Running the integration tests

You will need a fairly up-to-date version of rebar. If you fail to set the 
`skip_deps=true` option then the `rebar_retest_plugin` will not work. I am 
working on a fix for that particular issue.

    $ rebar skip_deps=true -C test.config get-deps retest -v
