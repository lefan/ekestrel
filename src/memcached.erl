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
-module(memcached).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([connect/1, connect/2, disconnect/1,
         set/3, set/5, setb/3, setb/5,
         cas/6, casb/6,
         get/2, getb/2, gets/2, getsb/2,
         get_multi/2, get_multib/2,
         gets_multi/2, gets_multib/2,
         replace/3, replace/5, replaceb/3, replaceb/5,
         add/3, add/5, addb/3, addb/5,
         append/3, prepend/3,
         delete/2,
         incr/3, decr/3,
         version/1,
         quit/1,
         stats/1,
         split/1,
         flush_all/1, flush_all/2
        ]).
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket}).

%%====================================================================
%% Definitions
%%====================================================================
-define(TCP_OPTIONS, [binary, {packet, raw}, {nodelay, true}, {active, false},
                      {sndbuf,16384},{recbuf,4096},{keepalive,true},{send_timeout,1000},{send_timeout_close,true}]).
-define(TIMEOUT, 10000).
-define(CR, 13).
-define(LF, 10).

-define(DEBUG(Format, Args),io:format("~s.~w: DEBUG: " ++ Format, [ ?MODULE, ?LINE | Args])).
%-define(DEBUG(Format, Args), true).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
    Host = proplists:get_value(hostname, Args),
    Port = proplists:get_value(port, Args),
    connect(Host, Port).

%% @spec connect(Host::string(), Port::integer()) -> {ok, Conn} | {error, Reason}
connect(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

connect(HostPortSpecs) ->
    [{Host, Port} | _More] = HostPortSpecs,
    gen_server:start_link(?MODULE, [Host, Port], []).

%%--------------------------------------------------------------------
%% Function: set
%% Description: set value
%% Returns: ok
%%--------------------------------------------------------------------
set(Conn, Key, Value) ->
    setb(Conn, Key, term_to_binary(Value)).
set(Conn, Key, Value, Flags, ExpTime) ->
    setb(Conn, Key, term_to_binary(Value), Flags, ExpTime).

%%--------------------------------------------------------------------
%% Function: setb
%% Description: set binary value
%% Returns: ok
%%--------------------------------------------------------------------
setb(Conn, Key, Value) when is_list(Key) andalso is_binary(Value) ->
    gen_server:call(Conn, {setb, Key, Value}).
setb(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_binary(Value) andalso is_integer(Flags) andalso is_integer(ExpTime) ->
    gen_server:call(Conn, {setb, Key, Value, Flags, ExpTime}).


%%--------------------------------------------------------------------
%% Function: cas
%% Description: set cas
%% Returns: ok
%%--------------------------------------------------------------------
cas(Conn, Key, Value, Flags, ExpTime, CasUnique64) ->
    casb(Conn, Key, term_to_binary(Value), Flags, ExpTime, CasUnique64).


%%--------------------------------------------------------------------
%% Function: casb
%% Description: set cas
%% Returns: ok
%%--------------------------------------------------------------------
casb(Conn, Key, Value, Flags, ExpTime, CasUnique64) when is_binary(Value) ->
    gen_server:call(Conn, {casb, Key, Value, Flags, ExpTime, CasUnique64}).


%%--------------------------------------------------------------------
%% Function: replace
%% Description: replace value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
replace(Conn, Key, Value) when is_list(Key) ->
    replaceb(Conn, Key, term_to_binary(Value)).
replace(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_integer(ExpTime) ->
    replaceb(Conn, Key, term_to_binary(Value), Flags, ExpTime).


%%--------------------------------------------------------------------
%% Function: replaceb
%% Description: replace binary value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
replaceb(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {replaceb, Key, Value}).
replaceb(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_integer(ExpTime) ->
    gen_server:call(Conn, {replaceb, Key, Value, Flags, ExpTime}).


%%--------------------------------------------------------------------
%% Function: add
%% Description: add value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
add(Conn, Key, Value) when is_list(Key) ->
    addb(Conn, Key, term_to_binary(Value)).
add(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_integer(ExpTime) ->
    addb(Conn, Key, term_to_binary(Value), Flags, ExpTime).


%%--------------------------------------------------------------------
%% Function: addb
%% Description: add binary value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
addb(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {addb, Key, Value}).
addb(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_integer(ExpTime) ->
    gen_server:call(Conn, {addb, Key, Value, Flags, ExpTime}).


%%--------------------------------------------------------------------
%% Function: append
%% Description: append value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
append(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {append, Key, Value}).


%%--------------------------------------------------------------------
%% Function: prepend
%% Description: prepend value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
prepend(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {prepend, Key, Value}).


%%--------------------------------------------------------------------
%% Function: get
%% Description: get value
%% Returns: {ok, Value}, {error, not_found} or {error, Reason}
%%--------------------------------------------------------------------
get(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {get, Key}).


%%--------------------------------------------------------------------
%% Function: gets
%% Description: get value and cas
%% Returns: {ok, Value, CasUnique64}, {error, not_found} or {error, Reason}
%%--------------------------------------------------------------------
gets(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {gets, Key}).


%%--------------------------------------------------------------------
%% Function: getb
%% Description: get value as binary
%% Returns: {ok, Value}, {error, not_found} or {error, Reason}
%%--------------------------------------------------------------------
getb(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {getb, Key}).


%%--------------------------------------------------------------------
%% Function: getsb
%% Description: get value as binary and cas
%% Returns: {ok, Value, CasUnique64}, {error, not_found} or {error, Reason}
%%--------------------------------------------------------------------
getsb(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {getsb, Key}).


%%--------------------------------------------------------------------
%% Function: get_multi
%% Description: get multiple values
%% Returns: {ok, Values}, Values = list of {Key, Value}.
%%--------------------------------------------------------------------
get_multi(Conn, Keys) when is_list(Keys) ->
    gen_server:call(Conn, {get_multi, Keys}).


%%--------------------------------------------------------------------
%% Function: get_multib
%% Description: get multiple binary values
%% Returns: {ok, Values}, Values = list of {Key, Value}.
%%--------------------------------------------------------------------
get_multib(Conn, Keys) when is_list(Keys) ->
    gen_server:call(Conn, {get_multib, Keys}).


%%--------------------------------------------------------------------
%% Function: gets_multi
%% Description: get multiple values with cas value
%% Returns: {ok, Values}, Values = list of {Key, Value, CasValue}.
%%--------------------------------------------------------------------
gets_multi(Conn, Keys) when is_list(Keys) ->
    gen_server:call(Conn, {gets_multi, Keys}).


%%--------------------------------------------------------------------
%% Function: gets_multib
%% Description: get multiple binary values with cas value
%% Returns: {ok, Values}, Values = list of {Key, Value}.
%%--------------------------------------------------------------------
gets_multib(Conn, Keys) when is_list(Keys) ->
    gen_server:call(Conn, {gets_multib, Keys}).


%%--------------------------------------------------------------------
%% Function: delete
%% Description: delete value
%% Returns: ok
%%--------------------------------------------------------------------
delete(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {delete, Key}).


%%--------------------------------------------------------------------
%% Function: incr
%% Description: incr value
%% Returns: {ok, NewValue}
%%--------------------------------------------------------------------
incr(Conn, Key, Value) when is_integer(Value) ->
    gen_server:call(Conn, {incr, Key, Value}).


%%--------------------------------------------------------------------
%% Function: decr
%% Description: decr value
%% Returns: {ok, NewValue}
%%--------------------------------------------------------------------
decr(Conn, Key, Value) when is_integer(Value) ->
    gen_server:call(Conn, {decr, Key, Value}).


%%--------------------------------------------------------------------
%% Function: version
%% Description: Returns memcached server version.
%% Returns: Version string
%%--------------------------------------------------------------------
version(Conn) ->
    gen_server:call(Conn, version).


%%--------------------------------------------------------------------
%% Function: stats
%% Description: Returns memcached stats
%% Returns: stats string
%%--------------------------------------------------------------------
stats(Conn) ->
    gen_server:call(Conn, stats, 10 * 1000).


%%--------------------------------------------------------------------
%% Function: quit
%% Description: Send quit command to server
%% Returns: quite
%%--------------------------------------------------------------------
quit(Conn) ->
    gen_server:call(Conn, quit).


%%--------------------------------------------------------------------
%% Function: flush_all
%% Description: Send flush_all command to server
%% Returns: quite
%%--------------------------------------------------------------------
flush_all(Conn) ->
    gen_server:call(Conn, flush_all).
flush_all(Conn, Sec) when is_integer(Sec) ->
    gen_server:call(Conn, {flush_all, Sec}).


%%--------------------------------------------------------------------
%% Function: disconnect
%% Description: disconnect
%% Returns: ok
%%--------------------------------------------------------------------
disconnect(Conn) ->
    gen_server:call(Conn, disconnect),
    ok.


init([Host, Port]) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS) of
        {ok, Socket} ->
            {ok, #state{socket=Socket}};
        {error, Reason} ->
            {stop, Reason};
        Other ->
            {stop, Other}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, Key}, _From, #state{socket=Socket}=State) ->
              case get_command(Socket, "get", [Key]) of
                  {ok, []} ->
                      {reply, {error, not_found}, State};
                  {ok, [{_Key, Value, _CasUnique64}]} ->
                      {reply, {ok, binary_to_term(Value)},  State};
                  Other ->
                      {stop, Other,  State}
              end;


handle_call({gets, Key}, _From, #state{socket=Socket}=State) ->
              case get_command(Socket, "gets", [Key]) of
                  {ok, []} ->
                      {reply, {error, not_found}, State};
                  {ok, [{_Key, Value, CasUnique64}]} ->
                      {reply, {ok, binary_to_term(Value), CasUnique64}, State};
                  Other ->
                      {stop, Other, State}
              end;


handle_call({getb, Key}, _From, #state{socket=Socket}=State) ->
              case get_command(Socket, "get", [Key]) of
                  {ok, []} ->
                      {reply, {error, not_found}, State};
                  {ok, [{_Key, Value, _CasUnique64}]} ->
                      {reply, {ok, Value}, State};
                  Other ->
                      {stop, Other, State}
              end;


handle_call({getsb, Key}, _From, #state{socket=Socket}=State) ->
              case get_command(Socket, "gets", [Key]) of
                  {ok, []} ->
                      {reply, {error, not_found}, State};
                  {ok, [{_Key, Value, CasUnique64}]} ->
                      {reply, {ok, Value, CasUnique64}, State};
                  Other ->
                      {stop, Other, State}
              end;


handle_call({get_multi, Keys}, _From, #state{socket=Socket}=State) ->
    case get_command(Socket, "get", Keys) of
        {ok, BinaryValues} ->
            Values = lists:map(fun({Key, Value, _CasUnique64}) -> {Key, binary_to_term(Value)} end, BinaryValues),
            {reply, {ok, Values}, #state{socket=Socket}=State};
        Other ->
            {stop, Other, #state{socket=Socket}=State}
    end;


handle_call({get_multib, Keys}, _From, #state{socket=Socket}=State) ->
    case get_command(Socket, "get", Keys) of
        {ok, BinaryValues} ->
            Values = lists:map(fun({Key, BinaryValue, _CasUnique64}) -> {Key, BinaryValue} end, BinaryValues),
            {reply, {ok, Values}, #state{socket=Socket}=State};
        Other ->
            {reply, Other, #state{socket=Socket}=State}
    end;


handle_call({gets_multi, Keys}, _From, #state{socket=Socket}=State) ->
    case get_command(Socket, "gets", Keys) of
        {ok, BinaryValues} ->
            Values = lists:map(fun({Key, Value, CasUnique64}) -> {Key, binary_to_term(Value), CasUnique64} end, BinaryValues),
            {reply, {ok, Values}, #state{socket=Socket}=State};
        Other ->
            {stop, Other, #state{socket=Socket}=State}
    end;


handle_call({gets_multib, Keys}, _From, #state{socket=Socket}=State) ->
    case get_command(Socket, "gets", Keys) of
        {ok, BinaryValues} ->
            Values = lists:map(fun({Key, Value, CasUnique64}) -> {Key, Value, CasUnique64} end, BinaryValues),
            {reply, {ok, Values}, #state{socket=Socket}=State};
        Other ->
            {stop, Other, #state{socket=Socket}=State}
    end;


handle_call({setb, Key, Value}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "set", Key, Value, 0, 0);
handle_call({setb, Key, Value, Flags, ExpTime}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "set", Key, Value, Flags, ExpTime);


handle_call({casb, Key, Value, Flags, ExpTime, CasUnique64}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "set", Key, Value, Flags, ExpTime, CasUnique64);


handle_call({replaceb, Key, Value}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "replace", Key, Value, 0, 0);
handle_call({replaceb, Key, Value, Flags, ExpTime}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "replace", Key, Value, Flags, ExpTime);

handle_call({addb, Key, Value}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "add", Key, Value, 0, 0);
handle_call({addb, Key, Value, Flags, ExpTime}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "add", Key, Value, Flags, ExpTime);


handle_call({append, Key, Value}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "append", Key, term_to_binary(Value), 0, 0);
handle_call({prepend, Key, Value}, _From, #state{socket=Socket}) ->
    storage_command(Socket, "prepend", Key, term_to_binary(Value), 0, 0);


handle_call({delete, Key}, _From, #state{socket=Socket}=State) ->
    {reply, delete_command(Socket, Key), #state{socket=Socket}=State};

handle_call({incr, Key, Value}, _From, #state{socket=Socket}=State) ->
    {reply, incr_decr_command(Socket, "incr", Key, Value), #state{socket=Socket}=State};
handle_call({decr, Key, Value}, _From, #state{socket=Socket}=State) ->
    {reply, incr_decr_command(Socket, "decr", Key, Value), #state{socket=Socket}=State};


handle_call(disconnect, _From, #state{socket=Socket}=State) ->
    {stop, normal, ok, #state{socket=Socket}=State};


handle_call(version, _From, #state{socket=Socket}=State) ->
    ok=gen_tcp:send(Socket, <<"version\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case split(binary_to_list(Packet)) of
                {Data, []} ->
                    case Data of
                        [$V | [$E | [$R | [$S | [$I | [$O | [$N | [32 | Version]]]]]]]] ->
                            {reply, Version, #state{socket=Socket}=State};
                        _ ->
                            {stop, {error, invalid_reseponse}, #state{socket=Socket}=State}
                    end;
                Other ->
                    {stop, wtfd, Other, #state{socket=Socket}=State}
            end;
        {error, Reason} ->
            {stop,Reason, {error, Reason}, #state{socket=Socket}=State}
    end;


handle_call(stats, _From, #state{socket=Socket}=State) ->
    ok=gen_tcp:send(Socket, <<"stats\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            Stats = parse_stats(binary_to_list(Packet), []),
            {reply, Stats, #state{socket=Socket}=State};
        {error, Reason} ->
            {stop, {error, Reason}, #state{socket=Socket}=State}
    end;


handle_call(quit, _From, #state{socket=Socket}=State) ->
    ok=gen_tcp:send(Socket, <<"quit\r\n">>),
    {stop, normal, ok, State};
%%     case get_socket(Key, Connections, CHash) of
%%         {ok, Socket, NewConnections} ->
%%             gen_tcp:send(Socket, <<"quit\r\n">>),
%%             {reply, ok, State};
%%         {error, Reason} ->
%%             {reply, {error, Reason}, State}
%%     end;


handle_call(flush_all, _From, #state{socket=Socket}=State) ->
    ok=gen_tcp:send(Socket, <<"flush_all\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case Packet of
                <<"OK\r\n">> ->
                    {reply, ok, #state{socket=Socket}=State};
                Other ->
                    {stop, {error, Other}, #state{socket=Socket}=State}
            end;
        Other ->
            {stop, {error, Other}, #state{socket=Socket}=State}
    end;


handle_call({flush_all, Sec}, _From, #state{socket=Socket}=State) ->
    Command = iolist_to_binary([<<"flush_all ">>, integer_to_list(Sec), <<"\r\n">>]),
    ok=gen_tcp:send(Socket, Command),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case Packet of
                <<"OK\r\n">> ->
                    {reply, ok, #state{socket=Socket}=State};
                Other ->
                    {stop, {error, Other}, #state{socket=Socket}=State}
            end;
        Other ->
            {stop, {error, Other}, #state{socket=Socket}=State}
    end.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("cast=~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(stop, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
    {stop, shutdown, State};
handle_info(Info, State) ->
    io:format("info=~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

parse_single_value(Data) ->
    case io_lib:fread("VALUE ~s ~u ~u", Data) of
        {ok, [Key, _Flags, Bytes], []} ->
            {Key, Bytes, []};
        _ ->
            case io_lib:fread("VALUE ~s ~u ~u ~u", Data) of
                {ok, [Key, _Flags, Bytes, CasUnique64], []} ->
                    {Key, Bytes, CasUnique64};
                Other ->
                    {error, Other}
            end
    end.


parse_values(Data) ->
    parse_values(Data, []).
parse_values(Data, Values) ->
    %% Format: VALUE <key> <flags> <bytes> [<cas unique>]\r\n
    case split(Data) of
        {error, Reason} ->
            {error, Reason};
        {"END", []} ->
            {ok, lists:reverse(Values)};
        {"ERROR", []} ->
            {error, unknown_command};
        {Head, Tail} ->
            case parse_single_value(Head) of
                {Key, Bytes, CasUnique64} ->
                    {ValueList, Rest}  = lists:split(Bytes, Tail),
                    Value = list_to_binary(ValueList),
                    case Rest of
                        [] -> {ok, lists:reverse([{Key, Value, CasUnique64} | Values])};
                        [?CR| [?LF | R]] ->
                            parse_values(R, [{Key, Value, CasUnique64} | Values])
                    end;
                Other ->
                    Other
            end
    end.


parse_stats(Data, Stats) ->
    case Data of
        "END\r\n" ->
            {ok, lists:reverse(Stats)};
        _ ->
            %% Format: VALUE <key> <flags> <bytes> [<cas unique>]\r\n
            case split(Data) of
                {error, Reason} ->
                    {error, Reason};
                {Head, Tail} ->
                    Parsed = io_lib:fread("STAT ~s ", Head),
                    {ok, [Key], Rest} = Parsed,
                    parse_stats(Tail, [{Key, Rest} | Stats])
            end
    end.

storage_command(Socket, Command, Key, Value, Flags, ExpTime) when is_integer(Flags) andalso is_integer(ExpTime) ->
    EmptyCasUnique64 = <<>>,
    storage_command(Socket, Command, Key, Value, Flags, ExpTime, EmptyCasUnique64).
storage_command(Socket, Command, Key, Value, Flags, ExpTime, CasUnique64) when is_integer(Flags) andalso is_integer(ExpTime) ->
              ValueAsBinary = Value,
              Bytes = integer_to_list(size(ValueAsBinary)),
              CommandAsBinary = iolist_to_binary([Command, <<" ">>, Key, <<" ">>, integer_to_list(Flags), <<" ">>, integer_to_list(ExpTime), <<" ">>, Bytes, <<" ">>, CasUnique64]),
              ok=gen_tcp:send(Socket, <<CommandAsBinary/binary, "\r\n">>),
              ok=gen_tcp:send(Socket, <<ValueAsBinary/binary, "\r\n">>),
              {reply,
               case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                   {ok, Packet} ->
                       case string:tokens(binary_to_list(Packet), "\r\n") of
                           ["STORED"] ->
                               ok;
                           ["NOT_STORED"] ->
                               {error, not_stored};
                           ["ERROR"] ->
                               {error, unknown_command};
                           %% memcached returns this for append command.
                           ["ERROR", "ERROR"] ->
                               {error, unknown_command};
                           Other ->
                               io:format("Other=~p~n", [Other]),
                               {error, Other}
                       end;
                   {error, Reason} ->
                       {error, Reason}
              end,
               #state{socket=Socket}}.


%% memcached 1.4.0 or higher doesn't support time argument.
delete_command(Socket, Key) ->
    Command = iolist_to_binary([<<"delete ">>, Key]),
    ok=gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, <<"DELETED\r\n">>} ->
            ok;
        {ok, <<"NOT_FOUND\r\n">>} ->
            {error, not_found};
        {ok, Other} ->
            {error, binary_to_list(Other)};
        {error, Reason} ->
            {error, Reason}
    end.

get_command(Socket, GetCommand, Keys) ->
    Command = iolist_to_binary([GetCommand, <<" ">>, string_join(" ", Keys)]),
    ok=gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            parse_values(binary_to_list(Packet));
        Other ->
            Other
    end.


incr_decr_command(Socket, IncrDecr, Key, Value) ->
    Command = iolist_to_binary([IncrDecr, " ", Key, " ", list_to_binary(integer_to_list(Value)), " "]),
    ok=gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case split(binary_to_list(Packet)) of
                {error, Reason} ->
                    {error, Reason};
                {"NOT_FOUND", []} ->
                   {error, not_found};
                {NewValueString, []} ->
                    case io_lib:fread("~u", NewValueString) of
                        {ok, [NewValue], []} ->
                            {ok, NewValue};
                        Other ->
                            {error, Other}
                    end;
                Other ->
                    {error, Other}
            end
    end.




%% filter_map(_Fun, []) ->
%%     [];
%% filter_map(Fun, [Elem | Rest]) ->
%%     case apply(Fun, [Elem]) of
%%         false ->
%%             filter_map(Fun, Rest);
%%         X ->
%%             [X | filter_map(Fun, Rest)]
%%     end.

%% Borrowed from http://www.trapexit.org/String_join_with
string_join(Join, L) ->
    string_join(Join, L, fun(E) -> E end).

string_join(_Join, L=[], _Conv) ->
    L;
string_join(Join, [H|Q], Conv) ->
    lists:flatten(lists:concat(
        [Conv(H)|lists:map(fun(E) -> [Join, Conv(E)] end, Q)]
    )).


split(Head, S) ->
    case S of
        [?CR | [?LF | More]] ->
            {lists:reverse(Head), More};
        [] ->
            {error, not_found};
        [H | T] ->
            split([H | Head], T)
    end.

%% split string with "\r\n"
split(S) ->
    split([], S).
