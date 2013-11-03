-module(tls_router_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("sni_parser/include/sni_parser.hrl").
-compile(export_all).

all() ->
    [
     route
    ].

init_per_suite(Config) ->
    application:load(foss),
    Config.

init_per_testcase(route=TestName, Config) ->
    RoutingTable = ets:new(routing_table, [public, named_table]),
    application:set_env(foss, routing_module, tl_ct_router),
    foss_app:start(),
    {Domain, Pid, Port} = create_backend(TestName),
    ets:insert(RoutingTable, {Domain, {"localhost", Port}}),
    [{routing_table, RoutingTable},
     {pid, Pid},
     {domain, Domain}|Config].

end_per_testcase(route, Config) ->
    application:stop(foss),
    ets:delete(?config(routing_table, Config)),
    Config.

%% Tests
route(Config) ->
    BackendPid = ?config(pid, Config),
    monitor(process, BackendPid),
    Port = foss_app:config(listening_port),
    Domain = ?config(domain, Config),
    os:cmd(io_lib:format("openssl s_client -connect localhost:~p -no_ssl3 -servername ~s", [Port, Domain])),
    erlang:monitor(process, BackendPid),
    receive
        {'DOWN', _, process, BackendPid, Reason} ->
            Reason = normal
    end,
    Config.

%% Helpers
create_backend(TestName) ->
    Domain = <<"test_domain">>,
    {ok, LSock} = gen_tcp:listen(0, [{active, false},binary]),
    {ok, Port} = inet:port(LSock),
    Pid = spawn(?MODULE, backend_server, [TestName, LSock, Domain]),
    {Domain, Pid, Port}.

backend_server(route, LSock, Domain) ->
    {ok, ASocket} = gen_tcp:accept(LSock),
    loop(ASocket, Domain).

loop(ASocket, Domain) ->
    inet:setopts(ASocket, [{active, once}]),
    receive
        {tcp, ASocket, Data} ->
            % The router should replay the client hello
            {ok, #client_hello{extensions = Extensions}} = sni_parse:parse(Data),
            SNI = proplists:get_value(sni, Extensions),
            ServerNameList = proplists:get_value(server_name_list, SNI),
            true = lists:member(Domain, ServerNameList);
        _ ->
            throw(failure)
    end.
