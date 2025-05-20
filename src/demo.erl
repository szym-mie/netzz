-module(demo).
-export([run/0]).

run() ->
    netpc:new(a0),
    netpc:new(b0),
    netpc:new(c0),
    netsw:new(s0),
    netdv:pair({a0, a0p1}, {s0, s0p1}),
    netdv:pair({b0, b0p1}, {s0, s0p2}),
    netdv:pair({c0, c0p1}, {s0, s0p3}),
    timer:sleep(1000),
    io:format("send from a0~n", []),
    Fab = neten:to_eth2(16#101010101010, 16#202020202020, 16#0800, <<"abcd">>),
    netdv:issue(a0, [send, a0p1, Fab]),
    timer:sleep(1000),
    io:format("send from b0~n", []),
    Fba = neten:to_eth2(16#202020202020, 16#101010101010, 16#0800, <<"dcba">>),
    netdv:issue(b0, [send, b0p1, Fba]),
    timer:sleep(1000).
