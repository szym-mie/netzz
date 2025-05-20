-module(netpc).
-export([new/1, init/1]).
-include("defs.hrl").

io_echo(Text) ->
    io:format("~p : ~p~n", [self(), Text]).

io_info(Dev) ->
    Ports = netdv:ports(Dev),
    io:format("~p pid ~p~nbinds:~n", [?MODULE, self()]),
    lists:foreach(
        fun(#port{port_id=PortId, to_bind=#bind{dev_id=DevId}}) -> 
            io:format("- ~p :: ~p~n", [PortId, DevId]) 
        end, Ports
    ).

handle_arp(Dev, PortId, _BindDst, Data) ->
    Arp = netpr:un_arp(Data),
    case Arp of
        {req, MacSen, IpSen, IpTgt} ->
            Frame = netpr:to_arp(MacSen, IpSen, IpTgt),
            netdv:unicast(Dev, PortId, Frame),
            {ok};
        _Else -> 
            {no_req}
    end.

data(Dev, {}, PortId, BindDst, Frame) ->
    io:format("PC: ~p <-- =~p [M]~n", [self(), PortId]),
    {_MacSrc, _MacDst, EthType, Data} = neten:un_eth2(Frame),
    case EthType of
        ?ARP_ETYPE -> handle_arp(Dev, PortId, BindDst, Data);
        _Else -> {bad_etype}
    end.

user(Dev, {}, Msg) ->
    try Msg of
        [send, PortId, Frame] ->
            netdv:unicast(Dev, PortId, Frame),
            io:format("PC: ~p ~p= --> [M]~n", [self(), PortId]);
            % neten:pr_eth2(Frame);
        [echo, Text] ->
            io_echo(Text);
        [info] ->
            io_info(Dev)
    catch
        error:Reason ->
            io:format("bad user msg ~p ~p~n", [self(), Reason])
    end.

new(DevId) ->
    Funcs = {fun data/5, fun user/3},
    Pid = spawn(netpc, init, [Funcs]),
    register(DevId, Pid),
    {ok, DevId}.

init(Funcs) ->
    Ports = ets:new(ports, [set, ?PORT_KEYPOS]),
    Conf = 0,
    netdv:run(#dev{ports=Ports, conf=Conf}, {}, Funcs).
