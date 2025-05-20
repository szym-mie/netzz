-module(netsw).
-export([new/1, init/1]).
-include("defs.hrl").

io_echo(Text) ->
    io:format("~p : ~p~n", [self(), Text]).

io_info(Dev, Fdb) ->
    Ports = netdv:ports(Dev),
    Fwds = ets:foldl(fun(Fwd, Acc) -> [Fwd | Acc] end, [], Fdb),
    io:format("~p pid ~p~nbinds:~n", [?MODULE, self()]),
    lists:foreach(
        fun(#port{port_id=PortId, to_bind=#bind{dev_id=DevId}}) -> 
            io:format("- ~p :: ~p~n", [PortId, DevId])
        end, Ports
    ),
    io:format("fdb:~n", []),
    lists:foreach(
        fun({Mac, Port}) ->
            io:format("- ~12.16.0B :: ~p~n", [Mac, Port])
        end, Fwds
    ).

fdb_get(Fdb, Mac) ->
    case ets:lookup(Fdb, Mac) of
        [#pfwd{mac_addr=Mac, port_id=PortId}] ->
            {ok, PortId};
        [] ->
            {no_fwd}
    end.

fdb_add(_Fdb, _PortId, ?MAC_BRDCAST) ->
    {bad_mac};
fdb_add(Fdb, PortId, Mac) ->
    ets:insert(Fdb, #pfwd{mac_addr=Mac, port_id=PortId}),
    {ok, PortId}.

data(Dev, {Fdb}, _PortIdSrc, BindDst, Frame) ->
    #bind{port_id=PortIdDst} = BindDst,
    {MacSrc, MacDst, _EthType, _Packet} = neten:un_eth2(Frame),
    % try register src MAC port
    fdb_add(Fdb, PortIdDst, MacSrc),
    % try to find port for dest MAC
    case fdb_get(Fdb, MacDst) of
        {ok, PortIdFwd} -> 
            % found port, send to port
            netdv:unicast(Dev, PortIdFwd, Frame),
            io:format("SW: ~p ~p= --> [M]~n", [self(), PortIdFwd]);
        {no_fwd} -> 
            % port not known/broadcast MAC, send to all ports
            netdv:broadcast(Dev, Frame),
            io:format("SW: ~p broad --> [M]~n", [self()])
    end.

user(Dev, {Fdb}, Msg) ->
    try Msg of
        [echo, Text] ->
            io_echo(Text);
        [info] ->
            io_info(Dev, Fdb)
    catch
        error:Reason ->
            io:format("bad user msg ~p ~p~n", [self(), Reason])
    end.

new(DevId) ->
    Funcs = {fun data/5, fun user/3},
    Pid = spawn(netsw, init, [Funcs]),
    register(DevId, Pid),
    {ok, DevId}.

init(Funcs) ->
    Ports = ets:new(ports, [set, ?PORT_KEYPOS]),
    Conf = 0,
    Fdb = ets:new(fdb, [set, ?PFWD_KEYPOS]),
    netdv:run(#dev{ports=Ports, conf=Conf}, {Fdb}, Funcs).
