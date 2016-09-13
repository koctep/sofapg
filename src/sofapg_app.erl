-module(sofapg_app).

-behaviour(application).
-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    application:ensure_started(cowboy),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Dispatch = fun(X) -> cowboy_router:compile([{'_', [{"/[...]", spg_router, X}]}]) end,
    cowboy:start_http(couch_api, 5, [{port, 15984}], [{env, [{dispatch, Dispatch({admin, false})}]}]),
    cowboy:start_http(couch_admin_api, 5, [{port, 15986}], [{env, [{dispatch, Dispatch({admin, true})}]}]),
    PgOpts = {"localhost", 5433, <<"sofapg">>, <<"sofapg">>, <<"sofapg">>},
    PoolSpec = poolboy:child_spec(pgpool,
                                  [{name, {local, pgpool}},
                                   {worker_module, spgdb},
                                   {size, 5},
                                   {max_overflow, 10}],
                                  PgOpts
                                 ),
    {ok, {{one_for_one, 5, 10}, [PoolSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
