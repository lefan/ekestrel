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
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Ekman=?CHILD(ekman,worker),
    Poolboy_sup=?CHILD_SUP(ek_pb_sup),
    {ok, { {one_for_all, 10, 10}, [Poolboy_sup, Ekman]} }.
