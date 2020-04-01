-module(pingpong).
-author("Agnieszka Dutka").

%% API
-export([start/0, stop/0, play/0]).

start()->
  register(ping, spawn(?MODULE, ping_fun(), [])),
  register(pong, spawn(?MODULE, pong_fun(), [])).

stop()->
  ping ! stop,
  pong ! stop.

play()->
  ping ! 5.

ping_fun()->
  receive
    _ -> io:format("ping");
    stop -> ok
  end.

pong_fun()->
  receive
    _ -> io:format("pong");
    stop -> ok
  end.