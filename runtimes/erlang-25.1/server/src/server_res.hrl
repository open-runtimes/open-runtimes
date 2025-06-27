-define(MIME_JSON, <<"application/json">>).
-define(MIME_TEXT, <<"text/plain">>).

-record(res, {
  status,
  headers = #{},
  body = ""
}).
