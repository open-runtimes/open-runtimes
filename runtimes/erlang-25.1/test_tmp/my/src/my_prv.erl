-module(my_prv).
-include("src/server_const.hrl").
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, my).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 my"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "example rebar3 my"},
            {desc, "example rebar3 my"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    % io:fwrite("U: ~p ~p ~n", [?USER_CONF_FILE, file:list_dir(?USER_CONF_FILE)]),
    UsrConf = rebar_config:consult_app_file(?USER_CONF_FILE),
    % io:fwrite("~p~n", [UsrConf]),

    case lists:keymember(deps, 1, UsrConf) of
      true ->
        % OldConf = rebar_config:consult_root(),
        OldConf = rebar_config:consult_file(?REBAR_CONFIG_SAMPLE),
        io:fwrite("~p~n", [OldConf]),

        U = proplists:get_value(deps, UsrConf), % element(2, lists:keyfind(deps, 1, UsrConf)),
        L = proplists:get_value(deps, OldConf), % element(2, lists:keyfind(deps, 1, OldConf)),

        % io:fwrite("C: ~p~n", [lists:keymerge(2, L, U)]),
        % io:fwrite("~p~n", [lists:keyfind(deps, 1, OldConf)]),
        % io:fwrite("L: ~p~n", [L]),
        io:fwrite("U: ~p~n", [U]),
        % F = lists:foldl(fun(T, Acc) ->
        %    case lists:keyfind(element(1, T), 1, L) of
        %      false ->
        %        Acc ++ [T];
        %      _ ->
        %        Acc
        %    end
        % end, L, U),
        F = merge_uniq(U, L),

        AppSrc = rebar_config:consult_app_file(?APP_SRC_SAMPLE),
        io:fwrite("~p~n", [AppSrc]),
        H = lists:nth(1, AppSrc),
        A = element(3, H),
        D = lists:foldl(fun(T, Acc) ->
            S = element(1, T),
            case lists:any(fun(E) -> E =:= S end, Acc) of
              true ->
                Acc;
              false ->
                Acc ++ [S]
            end
        end, proplists:get_value(applications, A), U),
        G = lists:keyreplace(applications, 1, A, {applications, D}),
        io:fwrite("G: ~p~n", [G]),
        I = setelement(3, H, G),
        io:fwrite("I: ~p~n", [I]),
        file:write_file(?APP_SRC, io_lib:format("~p.~n", [I])),

        % R = lists:map(fun(T) ->
        %        if
        %          element(1, T) == deps ->
        %            F;
        %          true ->
        %            T
        %        end
        %     end, OldConf),

        % io:fwrite("R: ~p~n", [R]),
        NewConf = lists:keyreplace(deps, 1, OldConf, {deps, F}),
        % maybe_merge_config(deps, 1, U, L),
        % file:write_file(?REBAR_CONFIG, io_lib:format("~p.~n", [NewConf])),
        % rebar_config:maybe_write_lock_file(?REBAR_CONFIG, NewConf, OldConf),

        Out = lists:foldl(fun(T, Acc) ->
            B = io_lib:format("~p.~n~n", [T]),
            string:concat(Acc, B)
            % <<Acc/binary, B/binary>>
        end, "", NewConf),
        % io:fwrite("Format: ~p~n", [Out]),
        % io:fwrite("Format: ~p~n", [lists:flatten(lists:join(".", NewConf))]),
        % file:write_file(?REBAR_CONFIG, io_lib:format("~p.~n", [lists:concat(lists:join(".", NewConf))])),
        file:write_file(?REBAR_CONFIG, Out),

        io:fwrite("NewConf: ~p~n", [NewConf]);
        % io:fwrite("NewConf: ~p~n", [io_lib:format("~p.~n", [{deps, [{1, "cow"}]}])]);
      _ ->
        io:fwrite("Nothing to modify in config~n")
    end,

    UsrLocks = rebar_config:consult_lock_file(?USER_LOCK_FILE),
    % io:fwrite("U Lock: ~p~n", [UsrLocks]),

    if
      length(UsrLocks) > 0 ->

        OldLocks = rebar_config:consult_lock_file(?REBAR_LOCK_SAMPLE),

        % io:fwrite("Lock: ~p~n", [OldLocks]),

        % NewLocks = lists:foldl(fun(T, Acc) ->
        %    case lists:keyfind(element(1, T), 1, OldLocks) of
        %      false ->
        %        Acc ++ [T];
        %      _ ->
        %        Acc
        %    end
        % end, OldLocks, UsrLocks),
        NewLocks = merge_uniq(UsrLocks, OldLocks),
        % rebar_config:maybe_write_lock_file(?REBAR_LOCK, NewLocks, OldLocks),
        io:fwrite("N Lock: ~p~n", [NewLocks]);

      true ->
        io:fwrite("Nothing to modify in lock file~n")
    end,

    % io:fwrite("~p~n", [?INTERNAL_RUNTIME_KEY]),

    % io:fwrite("~p~n", [file:list_dir(filename:join("..", ".."))]),
    % io:fwrite("~p~n", [filename:join("usr", "code", "server", "src", "server_const.hrl")]),
    % USER_CODE_PATH = filename:join("user", "code-start", os:getenv("INTERNAL_RUNTIME_KEY", "")),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

maybe_merge_config(Key, N, FromTupleList, ToTupleList) when length(FromTupleList) > 0 ->
  lists:keyreplace(Key, N, FromTupleList, {
    Key, merge_uniq(FromTupleList, ToTupleList)
  }).

merge_uniq(FromTupleList, ToTupleList) ->
    lists:foldl(fun(T, Acc) ->
       TupleKey = element(1, T),
       case lists:keyfind(TupleKey, 1, ToTupleList) of
         false ->
           Acc ++ [T];
         _ ->
           Acc
       end
    end, ToTupleList, FromTupleList).
