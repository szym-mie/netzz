-module(netdv).
-export([
    ports/1, port/2, def_mac/3, set_ip/4, set_ip/5, unicast/3, broadcast/3, 
    run/3, pair/2, issue/2
]).
-include("defs.hrl").

ports(Dev) ->
    ets:foldl(fun(Bind, Acc) -> [Bind | Acc] end, [], Dev#dev.ports).

port(Dev, PortId) ->
    case ets:lookup(Dev#dev.ports, PortId) of
        [Port] -> {ok, Port};
        [] -> {no_port}
    end.

def_mac(Dev, PortId, MacAddr) ->
    PrevPort = port(Dev, PortId),
    Port = PrevPort#port{mac_addr=MacAddr},
    ets:insert(Dev#dev.ports, Port),
    {ok}.

set_ip(Dev, PortId, IpAddr, IpMask) ->
    set_ip(Dev, PortId, IpAddr, IpMask, 0).

set_ip(Dev, PortId, IpAddr, IpMask, IpGate) ->
    PrevPort = port(Dev, PortId),
    Port = PrevPort#port{ip_addr=IpAddr, ip_mask=IpMask, ip_gate=IpGate},
    ets:insert(Dev#dev.ports, Port),
    {ok}.

bind(Dev, PortId, Bind) ->
    Port = #port{port_id=PortId, to_bind=Bind},
    ets:insert(Dev#dev.ports, Port),
    {ok}.

unicast(Dev, PortIdSrc, Frame) ->
    case port(Dev, PortIdSrc) of
        {ok, #port{to_bind=BindDst}} ->
            #bind{dev_id=DevIdDst} = BindDst,
            DevIdDst ! {data, PortIdSrc, BindDst, Frame},
            {ok, DevIdDst};
        {no_port} -> 
            {no_port}
    end.

broadcast(Dev, PortIdSrc, Frame) ->
    Ports = [Port || Port <- ports(Dev), Port#port.port_id =/= PortIdSrc],
    lists:foreach(
        fun(#port{port_id=PortIdDst}) -> 
            unicast(Dev, PortIdDst, Frame) 
        end, Ports
    ),
    {ok}.

run(Dev, State, Funcs) ->
    {DataFunc, UserFunc} = Funcs,
    receive
        {bind, PortId, Bind} ->
            bind(Dev, PortId, Bind),
            run(Dev, State, Funcs);
        {data, PortId, Bind, Frame} ->
            DataFunc(Dev, State, PortId, Bind, Frame),
            run(Dev, State, Funcs);
        {user, Msg} ->
            UserFunc(Dev, State, Msg),
            run(Dev, State, Funcs);
        {exit} ->
            io:format("exit dev ~p~n", [self()]),
            {exit}
    end.

pair({DevIdA, PortIdA}, {DevIdB, PortIdB}) ->
    DevIdA ! {bind, PortIdA, #bind{dev_id=DevIdB, port_id=PortIdB}},
    DevIdB ! {bind, PortIdB, #bind{dev_id=DevIdA, port_id=PortIdA}},
    {ok, DevIdA, DevIdB}.

    % try
    %     DevIdA ! {bind, DevIdB, PortB},
    %     DevIdB ! {bind, DevIdA, PortA},
    %     {ok, DevIdA, DevIdB}
    % catch
    %     error:badarg ->
    %         {bad_dest}
    % end.

issue(DevId, Msg) ->
    try
        DevId ! {user, Msg},
        {ok}
    catch
        error:badarg ->
            {bad_dest}
    end.
