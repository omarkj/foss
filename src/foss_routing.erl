-module(foss_routing).

-type socket() :: inet:socket().
-type transport() :: any().
-type reason() :: any().
-type state() :: any().
-type client_hello() :: any().
-type read_so_far() :: binary().

%% Incoming connection
-callback init(client_hello(), read_so_far()) ->
    {reject, reason()}|{accept, state()}.
-callback connection(socket(), transport(), state()) ->
    {stop, reason(), state()}|{ok, state()}.
-callback terminate(reason(), state()) ->
    any().
