import json
import os

SECRET = os.getenv("OPEN_RUNTIMES_SECRET", "")
ENTRYPOINT = os.getenv("OPEN_RUNTIMES_ENTRYPOINT")
ENV = os.getenv("OPEN_RUNTIMES_ENV")
# Inline logs travel on the response, so no log files are written
INLINE_LOGS = os.getenv("OPEN_RUNTIMES_LOGGING") == "inline"
LOGS_DIRECTORY = (
    "" if INLINE_LOGS else os.getenv("OPEN_RUNTIMES_LOGS_DIRECTORY", "/mnt/logs")
)

try:
    HEADERS = json.loads(os.getenv("OPEN_RUNTIMES_HEADERS", "") or "{}")
except ValueError:
    HEADERS = {}
