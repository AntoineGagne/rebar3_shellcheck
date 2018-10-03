shellcheck
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { shellcheck, ".*", {git, "git@host:user/shellcheck.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 shellcheck
    ===> Fetching shellcheck
    ===> Compiling shellcheck
    <Plugin Output>
