-module(spgdb).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).
-export([equery/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PGPOOL, pgpool).

-record(state, {worker}).

%%%===================================================================
%%% API functions
%%%===================================================================
equery(Query, Params) ->
  Exec = fun(Pid) ->
             gen_server:call(Pid, {equery, Query, Params})
         end,
  poolboy:transaction(?PGPOOL, Exec).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
  lager:debug("connecting to ~p", [Args]),
  gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Hostname, Database, Username}) ->
  init({Hostname, Database, Username, ""});
init({Hostname, Database, Username, Password}) ->
  init({Hostname, 5432, Database, Username, Password});
init({Hostname, Port, Database, Username, Password}) ->
  init({Hostname, Port, Database, Username, Password, []});
init({Hostname, Port, Database, Username, Password, Opts}) ->
  {ok, Pid} = epgsql:connect(Hostname, Username, Password, [{database, Database}, {port, Port}| Opts]),
  {ok, #state{worker = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({equery, Query, Params}, From, #state{worker = Pid} = State) ->
  Reply = epgsql:equery(Pid, Query, lists:map(fun normalize_param/1, Params)),
  gen_server:reply(From, Reply),
  {noreply, State};
handle_call(_Request, _From, State) ->
  lager:debug("unhandled call ~p: ~p", [_From, _Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
normalize_param(X) when is_map(X) ->
  jsxn:encode(X);
normalize_param(X) ->
  X.
