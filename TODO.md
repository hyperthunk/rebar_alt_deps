# Laundry List

## General

- Track cyclic dependencies
- Warn when cyclic dependencies exist
- Allow user to set up global and/or local policy for cyclic dependency handling
    - warn and continue when prior inclusion matches the version spec for an incoming (unprocessed) dependency
    - resolve to highest version
    - resolve to lowest version
    - first come, first served (i.e., resolve to the first version installed and ignore future inclusions)
- Support overriding config in deps

## SCM Handling

- Currently it's possible to checkout a repository at version ".*" :-)
