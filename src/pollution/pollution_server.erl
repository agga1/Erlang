-module(pollution_server).
-author("Agnieszka Dutka").

%% API
-export([start/0, stop/0, addStation/2, getMonitor/0, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getPeakHours/1, mostActiveStation/0]).
-export([init/0]).

start()-> register(pollution_server, spawn(?MODULE, init, [])).
init() -> loop(pollution:createMonitor()).
stop() -> call(stop).

call(Message) ->
  pollution_server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

loop(Monitor) ->
  receive
    {request, Pid, Message} ->
      case handle_call(Message, Pid, Monitor) of
        {reply, Reply, NewMonitor} ->
            Pid ! {reply, Reply},
            loop(NewMonitor);
        {stop, Reply, _ } ->
            Pid ! {reply, Reply}
      end
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
removeValue(StationId, Date, Type) -> call({removeValue, StationId, Date, Type}).
getOneValue(StationId, Date, Type) -> call({getOneValue, StationId, Date, Type}).
getStationMean(StationId, Type) -> call({getStationMean, StationId, Type}).
getDailyMean(Day, Type) -> call({getDailyMean, Day, Type}).
getPeakHours(Type) -> call({getPeakHours, Type}).
mostActiveStation() -> call({mostActiveStation}).

handle_call({getMonitor}, _From, Monitor) ->
  {reply, Monitor, Monitor};

handle_call({addStation, Name, Coord}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Monitor, Name, Coord),
  safe_return(NewMonitor, Monitor);

handle_call({addValue, Id, Date, Type, Value}, _From, Monitor) ->
  NewMonitor = pollution:addValue(Monitor,Id, Date, Type, Value),
  safe_return(NewMonitor, Monitor);

handle_call({removeValue, StationId, Date, Type}, _From, Monitor) ->
  NewMonitor = pollution:removeValue(Monitor, StationId, Date, Type),
  safe_return(NewMonitor, Monitor);

handle_call({getOneValue, StationId, Date, Type}, _From, Monitor) ->
  Value = pollution:getOneValue(Monitor, StationId, Date, Type),
  {reply, Value, Monitor};

handle_call({getStationMean, StationId, Type}, _From, Monitor) ->
  Value = pollution:getStationMean(Monitor, StationId, Type),
  {reply, Value, Monitor};

handle_call({getDailyMean, Day, Type}, _From, Monitor) ->
  Value = pollution:getDailyMean(Monitor,Day, Type),
  {reply, Value, Monitor};

handle_call({getPeakHours,Type}, _From, Monitor) ->
  Value = pollution:getPeakHours(Monitor, Type),
  {reply, Value, Monitor};

handle_call({mostActiveStation}, _From, Monitor) ->
  Value = pollution:mostActiveStation(Monitor),
  {reply, Value, Monitor};

handle_call(stop, _From, Monitor) ->
  {stop, normal, Monitor}.