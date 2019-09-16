# rebar3\_shellcheck

A `rebar3` plugin that runs `shellcheck` on given shell scripts.

## Usage

This plugin can found on [Hex](https://hex.pm/packages/rebar3_shellcheck).

Alternatively, you can add the plugin to your `rebar config` in the following
way:

```erlang
{plugins, [
    {shellcheck, ".*", {git, "git@github.com:AntoineGagne/rebar3_shellcheck.git", {tag, "v1.0.0"}}}
]}.
```

Then just call your plugin directly in an existing application:

```sh
rebar3 shellcheck
```
