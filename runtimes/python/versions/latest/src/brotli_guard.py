import sys

# CVE-2026-111497 (GHSA-9v8h-qqgm-4m35): the libbrotli decoder bundled in
# Brotli < 1.2.1 and brotlicffi < 1.2.1.0 writes out of bounds on a malformed
# stream. aiohttp loads whichever of the two the function installed (the
# server-env shares the runtime-env interpreter) and then decodes
# `Content-Encoding: br` request bodies while parsing, before the handler and
# its secret check run. The fix ships in the function's dependencies, not in
# this image, so when the installed package is vulnerable aiohttp is made to
# behave as if Brotli were missing: `br` request bodies get a 400, and aiohttp
# clients in user code stop advertising and decoding `br` responses.
FIXED_VERSIONS = {
    "brotli": (1, 2, 1),
    "brotlicffi": (1, 2, 1, 0),
}


def parse_version(version):
    parts = []
    for part in version.split("."):
        digits = ""
        for char in part:
            if not char.isdigit():
                break
            digits += char
        if not digits:
            break
        parts.append(int(digits))
    return tuple(parts)


def is_vulnerable(module):
    fixed = FIXED_VERSIONS.get(module.__name__)
    version = getattr(module, "__version__", "")
    # Unknown providers of the module (e.g. the abandoned brotlipy, which also
    # installs `brotli`) are treated as vulnerable
    return fixed is None or not version or parse_version(version) < fixed


def disable_vulnerable_brotli():
    from aiohttp import client_reqrep, compression_utils, hdrs, http_parser

    if not compression_utils.HAS_BROTLI:
        return

    module = compression_utils.brotli
    if not is_vulnerable(module):
        return

    # DeflateBuffer reads this flag on every request and response it decodes
    http_parser.HAS_BROTLI = False

    # The default Accept-Encoding was computed at import time with `br`
    default_headers = client_reqrep.ClientRequest.DEFAULT_HEADERS
    encodings = default_headers.get(hdrs.ACCEPT_ENCODING, "").split(",")
    default_headers[hdrs.ACCEPT_ENCODING] = ", ".join(
        encoding.strip() for encoding in encodings if encoding.strip() != "br"
    )

    print(
        "Warning: "
        + module.__name__
        + " "
        + getattr(module, "__version__", "unknown")
        + " is vulnerable to CVE-2026-111497, Brotli decoding is disabled. "
        + "Upgrade to Brotli>=1.2.1 or brotlicffi>=1.2.1.0.",
        file=sys.stderr,
    )
