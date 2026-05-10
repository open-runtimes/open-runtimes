-module(res).

-export([send/1, json/1]).

send(ResponseBody, Status = 200) ->
    { #{
        <<"content-type">> => <<"text/plain">>
    }, ResponseBody, Status }.

json(ResponseBody, Status = 200) ->
    { #{
        <<"content-type">> => <<"application/json">>
    }, ResponseBody, Status }.
