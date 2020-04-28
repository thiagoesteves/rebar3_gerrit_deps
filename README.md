rebar3_gerrit_deps
=====

    Rebar3 plugin for fetching dependencies from a gerrit + git repository.

How to Build
-----

    $ rebar3 compile

How to Use
---

Add the plugin to your `rebar.config`:

    {plugins, [
        {rebar3_gerrit_deps, ".*",
           {git, "git://github.com/thiagoesteves/rebar3_gerrit_deps.git", {branch, "master"}}}
    ]}.

Configure the dependencies to your `rebar.config`:

```erlang
{deps, [
  {my_repo , ".*", {gerrit, "ssh://gerrit/my_repo", {ref, "refs/changes/10/50000/1"}}}
  ..
]

```