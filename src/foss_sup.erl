-module(foss_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ListenerSpec = ranch:child_spec(tls_acceptor,
                                    foss_app:config(acceptors),
                                    ranch_tcp,
                                    [{port, foss_app:config(listening_port)}],
                                    foss_protocol, []),
    {ok, { {one_for_one, 5, 10}, [ListenerSpec]} }.

