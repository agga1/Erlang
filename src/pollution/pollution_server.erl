-module(pollution_server).
-author("Agnieszka Dutka").

%% API
-export([start/0, stop/0, addStation/2, getMonitor/0, addValue/4]).
-export([init/0]).

start()-> register(pollution_server, spawn(?MODULE, init, [])).

init() -> loop(pollution:createMonitor()).

stop() -> pollution_server ! stop.

call(Message) ->
  pollution_server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

loop(Monitor) ->
  receive
    {request, Pid, Message} ->
      {reply, Reply, NewMonitor} = handle_call(Message, Pid, Monitor),
      Pid ! {reply, Reply},
      loop(NewMonitor);
    stop -> ok
  end.

safe_return(NewMonitor, Monitor)->
  case NewMonitor of
    {error, _} ->
      {reply, NewMonitor, Monitor}; % return error message and keep previous state
    _ ->
      {reply, ok, NewMonitor}
  end.

getMonitor() -> call({getMonitor}).
addStation(Name, Coord) -> call({addStation, Name, Coord}).
addValue(Id, Date, Type, Value) -> call({addValue, Id, Date, Type, Value}).

handle_call({getMonitor}, _From, Monitor) ->
  {reply, Monitor, Monitor}; % return previous state

handle_call({addStation, Name, Coord}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Monitor, Name, Coord),
  safe_return(NewMonitor, Monitor);

handle_call({addValue, Id, Date, Type, Value}, _From, Monitor) ->
  NewMonitor = pollution:addValue(Monitor,Id, Date, Type, Value),
  safe_return(NewMonitor, Monitor).

