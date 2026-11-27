"""
Serves a WSGI app (Flask, Django, ...) under the Open Runtimes contract, the way
runtimes/javascript/src/ssr/http-injection.mjs does for Node servers: the runtime
endpoints, the secret check, per-execution logs, the execution timeout, and an
empty 500 for an uncaught error. The app itself stays unmodified.

gunicorn serves `open_runtimes_wsgi:application`. OPEN_RUNTIMES_WSGI names the
framework, which decides how the app is found from OPEN_RUNTIMES_ENTRYPOINT:

- flask: the `app` object of the entrypoint module (`app.py`, `src/app.py`).
- django: the project's WSGI_APPLICATION, found from `manage.py` the way
  `runserver` finds it, or the `application` of a `wsgi.py` entrypoint.

This module has its own name and no runtime imports, so a project with its own
`config` or `logger` module cannot shadow the runtime's.
"""

import contextvars
import importlib
import math
import os
import re
import signal
import sys
import threading
import traceback
from datetime import datetime

SECRET = os.getenv("OPEN_RUNTIMES_SECRET", "")
IS_DEVELOPMENT = os.getenv("OPEN_RUNTIMES_ENV") == "development"
LOGS_DIRECTORY = os.getenv("OPEN_RUNTIMES_LOGS_DIRECTORY", "/mnt/logs")
TIMINGS_FILE = "/mnt/telemetry/timings.txt"

TYPE_LOG = "logs"
TYPE_ERROR = "errors"

# The log id of the execution being served; "" while none is, or logging is off.
_log_id = contextvars.ContextVar("open_runtimes_log_id", default="")


def _generate_id(padding=7):
    now = datetime.now()
    base_id = f"{int(now.timestamp()):08x}{now.microsecond % 1000:05x}"
    return base_id + os.urandom(math.ceil(padding / 2)).hex()[:padding]


def _start_logging(status, log_id):
    if (status or "enabled") != "enabled":
        return ""

    if not log_id:
        log_id = "dev" if IS_DEVELOPMENT else _generate_id()

    return log_id


def _write(log_id, text, log_type):
    if not log_id or not text or not LOGS_DIRECTORY:
        return

    # Logging must never fail an execution
    try:
        with open(f"{LOGS_DIRECTORY}/{log_id}_{log_type}.log", "a") as file:
            file.write(text)
    except Exception:
        pass


class _Capture:
    """
    Stands in for sys.stdout / sys.stderr. What the app writes still reaches the
    container output, and also the log file of the execution being served:
    print() and logging go to its logs (stdout) or errors (stderr), as written.
    """

    def __init__(self, stream, log_type):
        self._stream = stream
        self._log_type = log_type

    def write(self, text):
        try:
            self._stream.write(text)
        except Exception:
            pass
        _write(_log_id.get(), text, self._log_type)
        return len(text)

    def __getattr__(self, name):
        return getattr(self._stream, name)


# Before the app is imported, so loggers it creates write through the capture too
sys.stdout = _Capture(sys.__stdout__, TYPE_LOG)
sys.stderr = _Capture(sys.__stderr__, TYPE_ERROR)


def _module_of(entrypoint):
    """`src/app.py` -> `src.app`"""
    path = entrypoint[:-3] if entrypoint.endswith(".py") else entrypoint
    return path.strip("/").replace("/", ".")


def _django_settings(manage_py):
    """
    The settings module `manage.py` sets, as in
    `os.environ.setdefault("DJANGO_SETTINGS_MODULE", "mysite.settings")`.
    """
    with open(manage_py) as file:
        match = re.search(
            r"""DJANGO_SETTINGS_MODULE['"]\s*,\s*['"]([\w.]+)['"]""", file.read()
        )
    return match.group(1) if match else None


def _load_app():
    framework = os.getenv("OPEN_RUNTIMES_WSGI", "")
    entrypoint = os.getenv("OPEN_RUNTIMES_ENTRYPOINT", "")

    if framework == "django" and os.path.basename(entrypoint) == "manage.py":
        settings_module = _django_settings(entrypoint)
        if settings_module:
            os.environ.setdefault("DJANGO_SETTINGS_MODULE", settings_module)

        import django
        from django.conf import settings

        django.setup(set_prefix=False)
        module_name, _, attribute = settings.WSGI_APPLICATION.rpartition(".")
    else:
        module_name = _module_of(entrypoint)
        attribute = "application" if framework == "django" else "app"

    return getattr(importlib.import_module(module_name), attribute)


_app = _load_app()


class _Timeout(BaseException):
    # A BaseException, so the framework's own error handling lets it through
    pass


def _on_timeout(signum, frame):
    raise _Timeout()


def _cancel_timeout():
    if threading.current_thread() is threading.main_thread():
        signal.setitimer(signal.ITIMER_REAL, 0)


def _text(start_response, status, body, headers=()):
    data = body.encode("utf-8")
    start_response(
        status,
        [("Content-Type", "text/plain"), ("Content-Length", str(len(data)))]
        + list(headers),
    )
    return [data]


def _error(start_response, log_id):
    return _text(
        start_response,
        "500 Internal Server Error",
        "",
        [("x-open-runtimes-log-id", log_id)],
    )


class _Body:
    """The app's response body, served with the execution's logs still captured."""

    def __init__(self, body, chunks, first, log_id):
        self._body = body
        self._chunks = chunks
        self._first = first
        self._log_id = log_id

    def __iter__(self):
        token = _log_id.set(self._log_id)
        try:
            if self._first is not None:
                yield self._first
            for chunk in self._chunks:
                yield chunk
        except Exception:
            # The response has started: log it, and let the server abort it
            _write(self._log_id, traceback.format_exc(), TYPE_ERROR)
            raise
        finally:
            _log_id.reset(token)

    def close(self):
        token = _log_id.set(self._log_id)
        try:
            close = getattr(self._body, "close", None)
            if close is not None:
                close()
        finally:
            _log_id.reset(token)


def application(environ, start_response):
    method = environ.get("REQUEST_METHOD", "GET")
    path = environ.get("PATH_INFO", "")

    if method == "GET" and path == "/__opr/health":
        return _text(start_response, "200 OK", "OK")

    if method == "GET" and path == "/__opr/timings":
        with open(TIMINGS_FILE) as file:
            return _text(start_response, "200 OK", file.read())

    if SECRET and environ.get("HTTP_X_OPEN_RUNTIMES_SECRET") != SECRET:
        return _text(
            start_response,
            "500 Internal Server Error",
            'Unauthorized. Provide correct "x-open-runtimes-secret" header.',
        )

    timeout = environ.get("HTTP_X_OPEN_RUNTIMES_TIMEOUT", "")
    safe_timeout = None
    if timeout:
        try:
            safe_timeout = float(timeout)
        except ValueError:
            safe_timeout = 0
        if not safe_timeout > 0:
            return _text(
                start_response,
                "500 Internal Server Error",
                'Header "x-open-runtimes-timeout" must be an integer greater than 0.',
            )

    log_id = _start_logging(
        environ.get("HTTP_X_OPEN_RUNTIMES_LOGGING"),
        environ.get("HTTP_X_OPEN_RUNTIMES_LOG_ID"),
    )
    token = _log_id.set(log_id)

    # The app's status and headers are held until its first chunk is ready, so a
    # body that fails before it can still be answered with an empty 500
    response = {}
    sent = []

    def send():
        if not sent:
            sent.append(start_response(response["status"], response["headers"]))
        return sent[0]

    def start_app_response(status, headers, exc_info=None):
        # The timeout covers the execution until its response starts, as for Node
        _cancel_timeout()
        if exc_info is not None and sent:
            raise exc_info[1].with_traceback(exc_info[2])
        response["status"] = status
        response["headers"] = [
            (name, value)
            for name, value in headers
            if name.lower() != "x-open-runtimes-log-id"
        ] + [("x-open-runtimes-log-id", log_id)]

        # The legacy write() callable: an app using it has started its response
        def write(data):
            return send()(data)

        return write

    # The timeout is a signal, which Python delivers in the main thread only: the
    # helpers run gunicorn's sync workers, so a request is served there. A custom
    # start command with threaded workers leaves the timeout to the executor.
    timed = (
        safe_timeout is not None
        and threading.current_thread() is threading.main_thread()
    )
    if timed:
        signal.signal(signal.SIGALRM, _on_timeout)
        signal.setitimer(signal.ITIMER_REAL, safe_timeout)

    try:
        body = _app(environ, start_app_response)
        chunks = iter(body)
        try:
            first = next(chunks)
        except StopIteration:
            first = None
        except Exception:
            # Nothing has reached the client yet; a later failure aborts the stream
            _write(log_id, traceback.format_exc(), TYPE_ERROR)
            close = getattr(body, "close", None)
            if close is not None:
                close()
            return _error(start_response, log_id)
        send()
        return _Body(body, chunks, first, log_id)
    except _Timeout:
        _write(log_id, "Execution timed out.\n", TYPE_ERROR)
        return _error(start_response, log_id)
    except Exception:
        _write(log_id, traceback.format_exc(), TYPE_ERROR)
        if sent:
            raise
        return _error(start_response, log_id)
    finally:
        _cancel_timeout()
        _log_id.reset(token)
