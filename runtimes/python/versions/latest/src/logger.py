import json
import sys
from datetime import datetime
import math
import os
from io import StringIO


class Logger:
    TYPE_ERROR = "error"
    TYPE_LOG = "log"

    id = ""
    enabled = False
    includes_native_info = False

    stream_logs = None
    stream_errors = None

    custom_std = None

    def __init__(self, status=None, id=None):
        if status is None or status == "":
            self.enabled = True
        else:
            if status == "enabled":
                self.enabled = True
            else:
                self.enabled = False

        if self.enabled is True:
            is_development = False
            if os.getenv("OPEN_RUNTIMES_ENV") == "development":
                is_development = True

            if id is None or id == "":
                if is_development is True:
                    self.id = "dev"
                else:
                    self.id = self.generate_id()
            else:
                self.id = id

            self.stream_logs = open("/mnt/logs/" + self.id + "_logs.log", "a")
            self.stream_errors = open("/mnt/logs/" + self.id + "_errors.log", "a")

    def write(self, messages, xtype=None, is_native=False):
        if xtype is None:
            xtype = Logger.TYPE_LOG

        if self.enabled is False:
            return

        if is_native is True and self.includes_native_info is False:
            self.includes_native_info = True
            self.write(
                [
                    "Native logs detected. Use context.log() or context.error() for better experience."
                ]
            )

        stream = self.stream_logs

        if xtype == Logger.TYPE_ERROR:
            stream = self.stream_errors

        string_log = ""
        i = 0
        for message in messages:
            if isinstance(message, (list, dict, tuple)):
                string_log += json.dumps(message, separators=(",", ":"))
            else:
                string_log += str(message)

            if i < len(messages) - 1:
                string_log += " "

            i += 1

        try:
            stream.write(string_log)
        except Exception as e:
            # Silently fail to prevent 500 errors in runtime
            # Log write failures should not crash the runtime
            pass

    def end(self):
        if self.enabled is False:
            return

        self.enabled = False

        self.stream_logs.close()
        self.stream_errors.close()

    def override_native_logs(self):
        sys.stderr = sys.stdout = self.custom_std = StringIO()

    def revert_native_logs(self):
        if self.custom_std is not None and self.custom_std.getvalue():
            self.write([self.custom_std.getvalue()], Logger.TYPE_LOG, True)

        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__

    def generate_id(self, padding=7):
        now = datetime.now()
        sec = int(now.timestamp())
        usec = now.microsecond % 1000
        base_id = f"{sec:08x}{usec:05x}"
        random_bytes = os.urandom(math.ceil(padding / 2))
        random_padding = random_bytes.hex()[:padding]
        return base_id + random_padding
