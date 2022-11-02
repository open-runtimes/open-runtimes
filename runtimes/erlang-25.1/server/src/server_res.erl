-module(server_res).
-include("src/server_app.hrl").

-export([new/3, send/1, send/2, json/1, json/2]).

new(Status, Headers, Body) ->
  #res{
    status = Status,
    headers = Headers,
    body = Body
  }.

send(ResponseBody, Status) ->
  new(Status, #{<<"content-type">> => ?MIME_TEXT}, ResponseBody).

send(ResponseBody) ->
  send(ResponseBody, 200).

json(ResponseBody, Status) ->
  new(Status, #{<<"content-type">> => ?MIME_JSON}, ResponseBody).

json(ResponseBody) ->
  json(ResponseBody, 200).
