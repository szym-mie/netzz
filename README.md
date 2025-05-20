# netzz

### net device is an erlang process

it's in the title, because, yes you can map a network device as a function, that will run in a different process.

### how to run a demo

make sure you have both `make` and `erl` installed on your computer.

run `make run` from root directory of this project, and just type `demo:run()` and hit enter. you should see some exchange of ethernet frames.

### what currently works

- PC - end device for sending and receiving network data
- SW - switch for exchanging frames between network devices

### explaining erlang processes

erlang has a very straightforward approach to concurrency: you can spawn new processes that execute a given function. such process is free from its parent and executes its own code alongside other procesess.

the real deal is that processes communicate via what is essentially email - each process has its own mailbox, can send messages to other processes and receive anytime it wants to. and it is delightful mostly, much better than dealing with threading explictly.

here i made a little demo that just countdowns to zero when you send it a message (frankly it's a bit unimaginative).

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
