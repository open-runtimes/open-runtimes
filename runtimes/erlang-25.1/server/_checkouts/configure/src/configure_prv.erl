-module(configure_prv).
-include("constants.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, configure).
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
            {example, "rebar3 configure"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Adjust dependencies in rebar config and lock file"},
            {desc, "Adjust dependencies in config and lock file"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:fwrite("===> Configure Start"),
    UsrConf = rebar_config:consult_app_file(?USER_CONF_FILE),

    case lists:keymember(deps, 1, UsrConf) of
      true ->
        OldConf = rebar_config:consult_file(?REBAR_CONFIG_SAMPLE),
        U = proplists:get_value(deps, UsrConf),
        L = proplists:get_value(deps, OldConf),
        F = merge_uniq(U, L),
        N = lists:keyreplace(deps, 1, OldConf, {deps, F}),
        NewConf = lists:foldl(fun(T, Acc) ->
            B = io_lib:format("~p.~n~n", [T]),
            string:concat(Acc, B)
        end, "", N),
        file:write_file(?REBAR_CONFIG, NewConf),

        AppSrc = rebar_config:consult_app_file(?APP_SRC_SAMPLE),
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
        I = setelement(3, H, G),
        file:write_file(?APP_SRC, io_lib:format("~p.~n", [I])),

        io:fwrite("~n===> Config File Updated");
      _ ->
        io:fwrite("~n===> Nothing to modify in Config File")
    end,

    UsrLocks = rebar_config:consult_lock_file(?USER_LOCK_FILE),

    if
      length(UsrLocks) > 0 ->
        OldLocks = rebar_config:consult_lock_file(?REBAR_LOCK_SAMPLE),
        NewLocks = merge_uniq(UsrLocks, OldLocks),
        rebar_config:maybe_write_lock_file(?REBAR_LOCK, NewLocks, OldLocks),

        io:fwrite("~n===> Lock File Updated");
      true ->
        io:fwrite("~n===> Nothing to modify in Lock File")
    end,

    io:fwrite("~n===> Configure Done~n"),

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

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
