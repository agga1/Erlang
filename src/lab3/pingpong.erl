-module(pingpong).
-author("Agnieszka Dutka").

%% API
-export([start/0, stop/0, play/1, ping_fun/1, pong_fun/0]).

start()->
  register(ping, spawn(?MODULE, ping_fun, [0])),
  register(pong, spawn(?MODULE, pong_fun, [])).

stop()->
  ping ! stop,
  pong ! stop.

play(N) when is_integer(N) andalso N>=0 ->
  ping ! N.

ping_fun(Sum)->
  receive
    N when is_integer(N), N >0 -> io:format("[ping] ~p, new state: ~p~n", [N, Sum +N]),
        timer:sleep(1000),
        pong ! N-1,
        ping_fun(Sum +N);
    stop -> ok
  after
    20000 -> ok
  end.

pong_fun()->
  receive
    N when is_integer(N), N >0 -> io:format("[pong] ~p~n", [N]),
      timer:sleep(1000),
      ping ! N-1,
      pong_fun();
    stop -> ok
  after
    20000 -> ok
  end.