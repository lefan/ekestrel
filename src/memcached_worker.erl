%%    Copyright (c) 2009-2010  Taro Minowa(Higepon) <higepon@users.sourceforge.jp>
%%
%%    Redistribution and use in source and binary forms, with or without
%%    modification, are permitted provided that the following conditions
%%    are met:
%%
%%    1. Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%
%%    2. Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%%    3. Neither the name of the authors nor the names of its contributors
%%       may be used to endorse or promote products derived from this
%%       software without specific prior written permission.
%%
%%    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%%-------------------------------------------------------------------
%%% File    : memcached.erl
%%% Author  : Taro Minowa(Higepon) <higepon@users.sourceforge.jp>
%%% Description : A minimal memcached client library.
%%%
%%% Created :  7 Dec 2009 by higepon <higepon@users.sourceforge.jp>
%%%-------------------------------------------------------------------
-module(memcached_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1]).

-record(state, {conn}).
%%====================================================================
%% Definitions
%%====================================================================
%%====================================================================
%% API
%%====================================================================
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    Host = proplists:get_value(hostname, Args),
    Port = proplists:get_value(port, Args),
    {ok, Conn} = memcached:connect(Host, Port),
    {ok, #state{conn=Conn}}.

handle_call({setb, Key, Value}, _From, #state{conn=Conn}=State) ->
    {reply, memcached:setb(Conn, Key, Value), State};
handle_call({setb, Key, Value, Flags, ExpTime}, _From, #state{conn=Conn}=State) ->
    {reply, memcached:setb(Conn, Key, Value, Flags, ExpTime), State};
handle_call({getb, Key}, _From, #state{conn=Conn}=State) ->
    {reply, memcached:getb(Conn, Key), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(stop, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    memcached:disconnect(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
