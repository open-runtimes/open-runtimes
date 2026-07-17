import json
import os

SECRET = os.getenv("OPEN_RUNTIMES_SECRET", "")
ENTRYPOINT = os.getenv("OPEN_RUNTIMES_ENTRYPOINT")
ENV = os.getenv("OPEN_RUNTIMES_ENV")

try:
    HEADERS = json.loads(os.getenv("OPEN_RUNTIMES_HEADERS", "") or "{}")
except ValueError:
    HEADERS = {}
