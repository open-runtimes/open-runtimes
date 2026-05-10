configure
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {configure, {git, "https://host/user/configure.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 configure
    ===> Fetching configure
    ===> Compiling configure
    <Plugin Output>
