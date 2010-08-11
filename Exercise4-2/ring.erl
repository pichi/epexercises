-module(ring).

-export([start/3, start_serial/3]).

% I have written a lot of ring implementation which sends messages in serial order.
% It means that in one time there is only one message on fly. For M=2 and N=3 it
% looked like: A -> B, B -> C, C -> A, A -> B, B -> C, C -> A
% Message were ordered.
% In this time I would like write version where those M messages would travel all
% in same time. This version is capable utilise more than one CPU core when former
% can utilize only one.

start(M, N, Message) ->
  Pid = start_ring(N, self()),
  [Pid ! {msg, Message} || _ <- lists:seq(1, M)],
  Pid ! quit,
  wait().

wait() ->
  receive
    quit -> ok;
    _ -> wait()
  end.

start_ring(0, Pid) -> Pid;
start_ring(N, Pid) ->
  start_ring(N - 1, spawn(fun() -> loop(Pid) end)).

loop(Pid) ->
  receive
    {msg, _} = Msg -> Pid ! Msg, loop(Pid);
    quit -> Pid ! quit, ok
  end.

% Serial version just for inspiration. Note that ring processes are exactly same.

start_serial(M, N, Message) ->
  Pid = start_ring(N, self()),
  send(Pid, M, Message),
  Pid ! quit,
  receive quit -> ok end.

send(_, 0, _) -> ok;
send(Pid, M, Message) ->
  Pid ! {msg, Message},
  receive % wait for return
    {msg, Message} -> send(Pid, M - 1, Message)
  end.
