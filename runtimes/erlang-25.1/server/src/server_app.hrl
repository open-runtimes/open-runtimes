-define(INTERNAL_RUNTIME_KEY, os:getenv("INTERNAL_RUNTIME_KEY")).
-define(INTERNAL_RUNTIME_ENTRYPOINT, os:getenv("INTERNAL_RUNTIME_ENTRYPOINT")).

-define(MIME_JSON, <<"application/json">>).
-define(MIME_TEXT, <<"text/plain">>).

-record(res, {
  status,
  headers = #{},
  body = ""
}).
