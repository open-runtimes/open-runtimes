-define(INTERNAL_RUNTIME_KEY, os:getenv("INTERNAL_RUNTIME_KEY")).
-define(INTERNAL_RUNTIME_ENTRYPOINT, os:getenv("INTERNAL_RUNTIME_ENTRYPOINT")).

-define(REBAR_CONFIG, "rebar.config").
-define(REBAR_LOCK, "rebar.lock").
-define(REBAR_CONFIG_SAMPLE, string:concat(?REBAR_CONFIG, ".sample")).
-define(REBAR_LOCK_SAMPLE, string:concat(?REBAR_LOCK, ".sample")).
-define(USER_CODE_PATH, filename:join(["/usr", "code", "example"])).
-define(USER_CONF_FILE, filename:join([?USER_CODE_PATH, ?REBAR_CONFIG])).
-define(USER_LOCK_FILE, filename:join([?USER_CODE_PATH, ?REBAR_LOCK])).
-define(APP_SRC, filename:join("src", "server.app.src")).
-define(APP_SRC_SAMPLE, string:concat(?APP_SRC, ".sample")).


-define(MIME_JSON, <<"application/json">>).
-define(MIME_TEXT, <<"text/plain">>).

-record(req, {
  variables = #{},
  headers = #{},
  payload = #{}
}).

-record(res, {
  status,
  headers = #{},
  body = ""
}).
