-module(sofapg).

-export([start/0]).
%%--------------------------------------------------------------------
%% @doc
%% Starts the application
%%
%% @spec start() -> {ok, Pid} | {ok, Pid, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    application:ensure_all_started(?MODULE),
    application:start(?MODULE).


