# rebar3\_shellcheck

A `rebar3` plugin that runs `shellcheck` on given shell scripts.

## Usage

Add the plugin to your `rebar config`:

```erlang
{plugins, [
    { shellcheck, ".*", {git, "git@github.com:AntoineGagne/rebar_shellcheck.git", {tag, "0.1.0"}}}
]}.
```

Then just call your plugin directly in an existing application:

```sh
rebar3 shellcheck
```
