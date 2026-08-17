import json
import os

SECRET = os.getenv("OPEN_RUNTIMES_SECRET", "")
ENTRYPOINT = os.getenv("OPEN_RUNTIMES_ENTRYPOINT")
ENV = os.getenv("OPEN_RUNTIMES_ENV")
LOGS_DIRECTORY = os.getenv("OPEN_RUNTIMES_LOGS_DIRECTORY", "/mnt/logs")

try:
    HEADERS = json.loads(os.getenv("OPEN_RUNTIMES_HEADERS", "") or "{}")
except ValueError:
    HEADERS = {}
