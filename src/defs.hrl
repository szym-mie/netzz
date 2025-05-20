%%% EtherTypes
-define(IPV4_ETYPE, 16#0800).
-define(ARP_ETYPE, 16#0806).
-define(VLAN_ETYPE, 16#8100).

%%% Address related
-define(MAC_BRDCAST, 16#ff_ff_ff_ff_ff_ff).
-define(MAC_SZ, 6).
-define(IPV4_SZ, 4).

%%% Device structs
-record(dev, {ports, conf}).
-record(bind, {dev_id, port_id}).
-record(port, {port_id, to_bind, mac_addr=0, ip_addr=0, ip_mask=0, ip_gate=0}).
-define(PORT_KEYPOS, {keypos, #port.port_id}).

%%% Frame & packets structs
-record(eth2, {mac_src, mac_dst, eth_type, data}).
-record(ipv4, {ip_src, ip_dst, ident, more_fra, fra_offset, ttl, proto, data}).

%%% Switch structs
-record(pfwd, {mac_addr, port_id}).
-define(PFWD_KEYPOS, {keypos, #pfwd.mac_addr}).
