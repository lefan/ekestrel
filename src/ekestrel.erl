-module(ekestrel).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0]).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).


start(_StartType, _StartArgs) ->
    start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(ekestrel, pools),
    PoolSpecs = lists:map(fun({PoolName, PoolConfig}) ->
				  Args = [{name, {local, PoolName}},
					  {worker_module, memcached}]
				      ++ PoolConfig,
				  {PoolName, {poolboy, start_link, [Args]},
				   temporary, 5000, worker, [poolboy]}
			  end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpecs} }.
