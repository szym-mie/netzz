-module(netpr).
-export([to_arp/3, to_arp/4, un_arp/1]).
-include("defs.hrl").

to_arp(MacSen, IpSen, IpTgt) ->
    <<
        1:16, ?IPV4_ETYPE:16, 
        ?MAC_SZ:8, ?IPV4_SZ:8, 1:16,
        MacSen:48, IpSen:32, 0:48, IpTgt:32
    >>.
to_arp(MacSen, IpSen, MacTgt, IpTgt) ->
    <<
        1:16, ?IPV4_ETYPE:16, 
        ?MAC_SZ:8, ?IPV4_SZ:8, 2:16,
        MacSen:48, IpSen:32, MacTgt:48, IpTgt:32
    >>.

un_arp(Data) ->
    <<
        1:16, ?IPV4_ETYPE:16, 
        ?MAC_SZ:8, ?IPV4_SZ:8, Oper:16,
        MacSen:48, IpSen:32, MacTgt:48, IpTgt:32
    >> = Data,
    case Oper of
        1 ->
            {req, MacSen, IpSen, IpTgt};
        2 ->
            {rpy, MacSen, IpSen, MacTgt, IpTgt}
    end.
