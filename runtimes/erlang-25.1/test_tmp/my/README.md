my
=====

example rebar3 my

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {my, {git, "https://host/user/my.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 my
    ===> Fetching my
    ===> Compiling my
    <Plugin Output>
