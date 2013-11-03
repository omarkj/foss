-module(foss_protocol).

-export([start_link/4,
         init/4]).

-record(handler, {module :: module(),
                  socket :: inet:socket(),
                  transport :: module(),
                  state :: any()}).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 5, 1000) of
        {ok, Data} ->
            case get_client_hello(Data, Socket, Transport) of
                ok ->
                    ok;
                {error, _} ->
                    ok = Transport:close(Socket)
            end;
        _ ->
            ok = Transport:close(Socket)
    end.

get_client_hello(Data, Socket, Transport) ->
    case sni_parse:parse(Data) of
        {ok, ClientHello} ->
            Mod = foss_app:config(routing_module),
            handler(init, Mod:init(ClientHello, Data), #handler{socket = Socket,
                                                                module = Mod,
                                                                transport = Transport});
        {error, {not_whole_handshake, ToRead}} ->
            case Transport:recv(Socket, ToRead, 1000) of
                {ok, Data1} ->
                    get_client_hello(<<Data/binary, Data1/binary>>,
                                     Socket, Transport);
                _ ->
                    {error, recv_failed}
            end;
        {error, Error} ->
            {error, Error}
    end.

handler(init, {reject, Reason}, #handler{module = Mod,
                                         state = State} = Handler) ->
    handler(terminate, Mod:terminate(Reason, State), Handler);
handler(init, {accept, NewState}, #handler{module = Mod,
                                           socket = Socket,
                                           transport = Transport} = Handler) ->
    handler(connection, Mod:connection(Socket, Transport, NewState), Handler#handler{state = NewState});
handler(connection, {ok, NewState}, #handler{module = Mod,
                                             socket = Socket,
                                             transport = Transport} = Handler) ->
    handler(connection, Mod:connection(Socket, Transport, NewState), Handler#handler{state = NewState});
handler(connection, {stop, Reason, NewState}, #handler{module = Mod} = Handler) ->
    handler(terminate, Mod:terminate(Reason, NewState), Handler#handler{state = NewState});
handler(terminate, _, #handler{socket = Socket,
                               transport = Transport}) ->
    catch Transport:close(Socket).
