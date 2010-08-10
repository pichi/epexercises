-module(echo).

-export([start/0, stop/0, print/1, loop/0]).

start() ->
  Pid = spawn(?MODULE, loop, []),
  try register(?MODULE, Pid) of
    true -> ok
  catch
    error:badarg -> Pid ! stop
  end.

stop() ->
  ?MODULE ! stop,
  ok.

print(X) ->
  ?MODULE ! {print, X},
  ok.

loop() ->
  receive
    stop -> ok;
    {print, X} ->
      io:format("~p~n", [X]),
      ?MODULE:loop();
    Msg ->
      io:format("[~p] Unexpected message: ~p~n", [?MODULE, Msg])
  end.
