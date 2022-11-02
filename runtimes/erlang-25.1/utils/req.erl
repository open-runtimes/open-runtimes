-module(req).

-export([new/1]).

new_req(Req) ->
  {ok, Data, Req0} = cowboy_req:read_body(Req).
  Data0 = jsone:decode(Data)
  #{
    :variables => Data0.variables,
    :headers => cowboy_req:headers(Req),
    :payload => Data0.payload}.
