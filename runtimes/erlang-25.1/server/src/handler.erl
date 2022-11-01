-module(handler).

-export([main/1]).

main({_, V, H, P } = Req) ->
  io:fwrite("~nReq: ~w ~n~n", [Req]),
  io:fwrite("Vars: ~w Headers: ~w Payload: ~w~n~n", [V, H, P]),
  % io:fwrite("Req Headers: ~w~n", [maps:get(headers, Req)]),
  % throw(<<"Something happend">>),
  % server_res:send(<<"Hi">>).
  server_res:json(#{ data => <<"Some Data">> }).
