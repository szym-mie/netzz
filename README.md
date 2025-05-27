# netzz

### net device is an erlang process

### how to run a demo

Make sure you have both `make` and `erl` installed on your computer.

Run `make run` from root directory of this project, and just type `demo:run()` and hit enter. you should see some exchange of ethernet frames.

### what currently works

#### PC

End device for sending and receiving network data

- just send frames (no layer 3)


#### SW

Switch for exchanging frames between network devices

- supports FDB, unicast and broadcast ethernet frames
- no support for spanning tree yet, switching loops are possible

### confusing parts

#### port id src/dst

It's something that I myself get wrong all the time:
```
send frame from [a0] thru [s0]:
                                   ...
            -> [frame] ->          /
[ PC:a0 ]-a0p0---------s0p0-[ SW:s0 ]
           |             |         \
          src           dst        ...
        port id       port id
```

Source and destination are viewed from an "external" perspective of the link. This perspective is used throught the code.

There is a different perspective of the network device: for the switch `s0`, `s0p0` is a src port (inbound) and for the PC `a0`, `a0p0` port is dst (outbound).

### explaining erlang processes

Erlang has a very straightforward approach to concurrency: you can spawn new processes that execute a given function. such process is free from its parent and executes its own code alongside other procesess.

The real deal is that processes communicate via what is essentially email - each process has its own mailbox, can send messages to other processes and receive anytime it wants to. and it is delightful mostly, much better than dealing with threading explictly.

Here I made a little demo that just countdowns to zero when you send it a message (frankly it's a bit unimaginative).

```erl
-module(countdown).
-export([start/1, count/2]).

countdown(Pid) ->
    countdown(Pid, 0).
countdown(Pid, 0) ->
    Pid ! {exit},
    receive
        {count, StartCount} ->
            countdown(Pid, StartCount)
    end;
countdown(Pid, Count) ->
    Pid ! {down, Count},
    countdown(Pid, Count - 1).

start(Name) ->
    Pid = spawn(countdown, countdown, [self()]),
    register(Pid, Name),
    {ok, Pid}.

wait_count() ->
    receive
        {down, Count} ->
            io:format("count: ~p~n", [Count]),
            wait_count();
        {exit} ->
            io:format("count: exit~n", [])
    end.

count(Proc, StartCount) ->
    Proc ! {count, StartCount},
    wait_count().
```

This approach is not unique however: Ray framework for Python works on the same princple, although it doesn't utilize anything similar to `receive`, making it much more prone to deadlocks compared to Erlang with its pattern matching.
