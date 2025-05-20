-module(neten).
-export([to_eth2/4, to_ipv4/8, un_eth2/1, un_ipv4/1, pr_eth2/1]).
-inlcude("defs.hrl").

%%% net* encapsulation functions: Ethernet II, Internet Protocol 4
%%% in the future: VLANs, fragmentation

to_eth2(MacSrc, MacDst, EthType, Data) ->
    %% skips Frame Check Sequence - no function, complicates patterns
    <<MacDst:48, MacSrc:48, EthType:16, Data/binary>>.

to_ipv4(IpSrc, IpDst, Ident, _MoreFra, FraOffset, Ttl, Proto, Data) ->
    %% skips Checksum - no function, complicates patterns
    Len = byte_size(Data) + 20,
    FraInfo = 0, % MoreFra bsl 2,
    <<
        4:4, 5:4, 0:8, Len:16,
        Ident:16, FraInfo:3, FraOffset:13,
        Ttl:8, Proto:8, 0:16,
        IpSrc:32, IpDst:32, Data/binary
    >>.

un_eth2(Frame) ->
    <<MacDst:48, MacSrc:48, EthType:16, Data/binary>> = Frame,
    {MacSrc, MacDst, EthType, Data}.

un_ipv4(Packet) ->
    <<
        4:4, 5:4, 0:8, _Len:16,
        Ident:16, _FraInfo:3, FraOffset:13,
        Ttl:8, Proto:8, 0:16,
        IpSrc:32, IpDst:32, Data/binary
    >> = Packet,
    {IpSrc, IpDst, Ident, 0, FraOffset, Ttl, Proto, Data}.

pr_eth2(Frame) ->
    {MacSrc, MacDst, EthType, _Data} = un_eth2(Frame),
    FormatArgs = [MacSrc, MacDst, EthType],
    io:format("ETH2 src ~12.16.0B dst ~12.16.0B type ~8.16.0B~n", FormatArgs).
