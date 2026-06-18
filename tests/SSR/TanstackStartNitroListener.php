<?php

namespace Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

// Nitro node-listener preset: served via the express wrapper, which (unlike the
// standalone build) serves static assets itself, so testStaticCache is kept.
class TanstackStartNitroListener extends TanstackStart
{
}
