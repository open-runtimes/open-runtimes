-module(server_handler).
-include("src/server_app.hrl").

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2
]).

-export([handle_req/2]).
 
init(Req, State) ->
    {cowboy_rest, Req, State}.
    % case Method of
    %     <<"POST">> ->
    %         handle_req(Req);
    %     _ ->
    %         cowboy_req:reply(404, Req)
    % end.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, handle_req}
    ], Req, State}.

handle_req(Req, _State) ->
    io:fwrite("=:= HERE =:=~n"),
    % #{headers := Headers} = Req,
    % Headers = cowboy_req:headers(Req),
    % io:fwrite("~s~n", [Headers]),
    % C = maps:get(<<"x-internal-challenge">>, Headers),
    % io:fwrite("~w~n", [C]),
    % V = os:getenv("INTERNAL_RUNTIME_KEY"),
    io:fwrite("~w~n", [?INTERNAL_RUNTIME_KEY]),
    % R = string:equal(
    %     maps:get(<<"x-internal-challenge">>, Headers),
    %     os:getenv("INTERNAL_RUNTIME_KEY")
    % ),

    % io:fwrite("~w ~w~n", [cowboy_req:has_body(Req), R]),
    io:fwrite("HEADER: ~w~n", [cowboy_req:header(<<"x-internal-challenge">>, Req)]),

    case cowboy_req:has_body(Req) of
      true ->
        case string:equal(
            % maps:get(<<"x-internal-challenge">>, Headers),
            cowboy_req:header(<<"x-internal-challenge">>, Req),
            ?INTERNAL_RUNTIME_KEY
        ) of
          true ->
            {ok, Body, _} = cowboy_req:read_body(Req),

            io:fwrite("Body: ~w~n", [Body]),
            Body0 = jsone:decode(Body),
            io:fwrite("Body0: ~w~n", [Body0]),
            Req0 = #{
              variables => maps:get(variables, Body0, #{}),
              headers => maps:get(headers, Body0, #{}),
              payload => maps:get(payload, Body0, #{})
            },

            Res = try
                handler:main(Req0)
            catch
               Class:Reason:Stacktrace ->
                  erlang:display(Stacktrace),
                  % {caught, Exception, Reason},
                  % [{M,F,As,Info}|_] = Stacktrace,
                  % io:fwrite("Checking: ~p ~p ~p~n", [M, F, As]),
                  % ErrorInfo = proplists:get_value(error_info, Info, #{}),
                  % io:fwrite("Checking: ~p~n", [ErrorInfo]),
                  % ErrorMap = maps:get(cause, ErrorInfo, #{}),
                  % io:fwrite("Checking: ~p~n", [ErrorMap]),
                  % io:fwrite("Checking: ~p~n", [ErrorMap#{
                  %   general => "optional general information",
                  %   reason => io_lib:format("~p: ~p",[?MODULE, Reason])
                  % }]),
                  Message = erl_error:format_exception(Class, Reason, Stacktrace, #{
                    stack_trim_fun => fun(M, A, _) ->
                      % io:fwrite("My: ~p:~p~n~n", [M, A]),
                      not (handler == M andalso main == A)
                    end
                  }),
                  % io:fwrite("Checking: ~w ~w ~p~n", [caught, Class, Reason]),
                  io:fwrite("~n~n*** Error ***:~n~ts~n", [Message]),
                  server_res:send(erlang:iolist_to_binary(Message), 500)
               % _:_:Stacktrace ->
               %    io:fwrite("Failed"),
               %    erlang:display(Stacktrace),
               %    % cowboy_req:reply(500, Req)
               %    server_res:send(<<"Failed in Code">>, 500)
            end,

            io:fwrite("RESPONSE: ~w~n", [Res]),

            Output = maps:merge(#{
                stdout => ""
            },
              if
                Res#res.status == 500 ->
                  #{stderr => Res#res.body};
                true ->
                  #{response => Res#res.body}
              end
            ),

            % maps:set("response", Res#res.body, Output),
            % maps:set("stderr", Res#res.body, Output),

            % io:fwrite("RESULT: ~w ~w ~w~n", [Res#res.headers, Res#res.body, jsone:encode("Hi")]),
            % io:fwrite("RESULT: ~w~n", [Output]),
            {ok, cowboy_req:reply(Res#res.status, Res#res.headers, jsone:encode(Output), Req)};
          false ->
            cowboy_req:reply(401, #{}, jsone:encode(#{
                stderr => 'Unauthorized'
            }), Req)
        end;
      false ->
        cowboy_req:reply(400, #{}, jsone:encode(#{
            stderr => 'Bad Request'
        }), Req)
    end.

% format_error(Reason, [{_M,_F,_As,Info}|_]) ->
%   ErrorInfo = proplists:get_value(error_info, Info, #{}),
%   ErrorMap = maps:get(cause, ErrorInfo),
%   ErrorMap#{ general => "optional general information",
%              reason => io_lib:format("~p: ~p",[?MODULE, Reason]) }.
