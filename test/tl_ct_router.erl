-module(tl_ct_router).
-behaviour(foss_routing).
-record(state, {hello,
                backend}).
-include_lib("sni_parser/include/sni_parser.hrl").

-export([init/2,
         connection/3,
         terminate/2]).

init(#client_hello{extensions = Extensions} = ClientHello, Data) ->
    case proplists:get_value(sni, Extensions) of
        undefined ->
            {reject, no_sni};
        List ->
            ServerNames = proplists:get_value(server_name_list, List),
            case find_backend(ServerNames) of
                no_match ->
                    {reject, no_match};
                {Server, Port} ->
                    {ok, BackendSocket} = gen_tcp:connect(Server, Port, [{active, false}]),
                    gen_tcp:send(BackendSocket, Data),
                    {accept, #state{hello = ClientHello,
                                    backend = BackendSocket}}
            end
    end.

connection(ClientSocket, Transport, #state{backend=BackendSocket}=State) ->
    Transport:setopts(ClientSocket, [{active, once}]),
    Transport:setopts(BackendSocket, [{active, once}]),
    receive
        {tcp, ClientSocket, Data} ->
            case Transport:send(BackendSocket, Data) of
                ok ->
                    {ok, State};
                {error, Error} ->
                    {stop, Error, State}
            end;
        {tcp, BackendSocket, Data} ->
            case Transport:send(ClientSocket, Data) of
                ok ->
                    {ok, State};
                {error, Error} ->
                    {stop, Error, State}
            end;
        {tcp_closed, BackendSocket} ->
            catch Transport:close(ClientSocket),
            {stop, normal, State};
        {tcp_closed, ClientSocket} ->
            catch Transport:close(BackendSocket),
            {stop, normal, State}
    end.

terminate(_, _) ->
    ok.

find_backend([]) ->
    no_match;
find_backend([ServerName|Rest]) ->
    case ets:lookup(routing_table, ServerName) of
        undefined ->
            find_backend(Rest);
        [{_, Port}] ->
            Port
    end.
