%%%-------------------------------------------------------------------
%%% @author Alexey Larin <>
%%% @copyright (C) 2012, Alexey Larin
%%% @doc
%%%
%%% @end
%%% Created :  6 Apr 2012 by Alexey Larin <>
%%%-------------------------------------------------------------------
-module(ekman).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([rr_get_active_pool/0,get_any_active_pool/0]).

-define(SERVER, ?MODULE). 

-record(state, {rr_queue}).

-define(DEBUG(Format, Args),io:format("~s.~w: DEBUG: " ++ Format, [ ?MODULE, ?LINE | Args])).
%-define(DEBUG(Format, Args), true).
-define(WDTIME,15000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

rr_get_active_pool() ->
    gen_server:call(?SERVER,rr_get_active_pool).

get_any_active_pool() ->
    Pools=[P||[P] <- ets:match(ek_pools_active,{'_','$1'})],
    lists:nth(random:uniform(length(Pools)), Pools).
    
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
init([]) ->
    ets:new(ek_pools_cfg,[protected,named_table]),
    ets:new(ek_pools_active,[protected,named_table]),
    load_pools_cfg(),
    start_all_pools(),
    Q=rebuild_rr_queue(),
    WDT=?WDTIME,
    erlang:send_after(WDT,self(),{cmontime,WDT}),
    %%?DEBUG("PoolArgs: ~p~n",[PoolArgs]),
    %% [Arg|_]=PoolArgs,
    %% ?DEBUG("Args: ~p~n",[Args]),
    %% [?DEBUG("Args: ~p~n",[Args])|| Args <- PoolArgs],
    %% Repl=[supervisor:start_child(ek_pb_sup, [Args])|| Args <- PoolArgs],
    %% ?DEBUG("Repl: ~p~n",[Repl]),
    {ok, #state{rr_queue=Q}}.

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
handle_call(rr_get_active_pool, _From, #state{rr_queue=Q1}=State) ->
    case queue:out(Q1) of
	{{value, Item}, Q2} ->
	    Q3=queue:in(Item,Q2),
	    {reply,Item,State#state{rr_queue=Q3}};
	{empty, Q1} ->
	    {reply, empty, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({'DOWN',_MonitorRef,process,Pid, Err},State) ->
    ?DEBUG("Recived DOWN msg: ~p",[{'DOWN',_MonitorRef,process,Pid, Err}]),
    ets:delete(ek_pools_active,Pid),
    {noreply, State#state{rr_queue=rebuild_rr_queue()}};
handle_info({cmontime,NextTime},State) ->
    restart_missed_pools(),
    test_all_conn(),
    %?DEBUG("Recived cmontime with NextTime=~p~n",[NextTime]),
    erlang:send_after(NextTime,self(),{cmontime,NextTime}),
    {noreply, State#state{rr_queue=rebuild_rr_queue()}};

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

start_pool(PoolName)->
    case ets:lookup(ek_pools_cfg,PoolName) of
	[{PoolName,Args}] ->
	    case supervisor:start_child(ek_pb_sup, [Args]) of
		{ok,Pid} ->
		    ets:insert(ek_pools_active,{Pid,PoolName}),
		    erlang:monitor(process,Pid),
		    {ok,Pid};
		Other -> Other
	    end;
	[] -> unknown_pool
    end.


load_pools_cfg() ->
    {ok, Pools} = application:get_env(ekestrel, pools),
    lists:map(fun({PoolName, PoolConfig}) ->
				 Args=[{name, {local, PoolName}},
				       {worker_module, memcached}]
				     ++ PoolConfig,
				 ets:insert(ek_pools_cfg,{PoolName,Args})
			 end, Pools).

start_all_pools() ->
    [start_pool(PoolName)||[PoolName] <- ets:match(ek_pools_cfg,{'$1','_'})].

restart_missed_pools() ->
    [start_pool(PoolName)||[PoolName]<- ets:match(ek_pools_cfg,{'$1','_'})--ets:match(ek_pools_active,{'_','$1'})].
		    
test_all_conn() ->
    catch
    [[memcached:version(WorkerPid)||WorkerPid <- gen_fsm:sync_send_all_state_event(PoolPid,get_avail_workers)]||[PoolPid]<- ets:match(ek_pools_cfg,{'$1','_'})].
    
rebuild_rr_queue() ->
    queue:from_list([PoolName||[PoolName]<- ets:match(ek_pools_active,{'_','$1'})]).
    
