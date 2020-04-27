-module(pollution_gen_server).
-author("Agnieszka Dutka").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
%% API
-export([start_link/0, close/0, crash/0, getMonitor/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getPeakHours/1, mostActiveStation/0]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

start_link()   -> gen_server:start_link({local,?SERVER},?MODULE,[],[]).
init(_) ->   {ok, pollution:createMonitor()}.

getMonitor() -> gen_server:call(?SERVER, getMonitor).
addStation(Name, Coord) -> gen_server:call(?SERVER, {addStation, Name, Coord}).
addValue(Id, Date, Type, Value) -> gen_server:call(?SERVER,{addValue, Id, Date, Type, Value}).
removeValue(StationId, Date, Type) -> gen_server:call(?SERVER,{removeValue, StationId, Date, Type}).
getOneValue(StationId, Date, Type) -> gen_server:call(?SERVER,{getOneValue, StationId, Date, Type}).
getStationMean(StationId, Type) -> gen_server:call(?SERVER,{getStationMean, StationId, Type}).
getDailyMean(Day, Type) -> gen_server:call(?SERVER,{getDailyMean, Day, Type}).
getPeakHours(Type) -> gen_server:call(?SERVER,{getPeakHours, Type}).
mostActiveStation() -> gen_server:call(?SERVER,{mostActiveStation}).

close()     -> gen_server:call(?SERVER,terminate).
crash()     -> gen_server:cast(?SERVER,crash).


safe_return(NewMonitor, Monitor)->
  case NewMonitor of
    {error, _} ->
      {reply, NewMonitor, Monitor}; % return error message and keep previous state
    _ ->
      {reply, ok, NewMonitor}
  end.
%% handling messages %%
handle_cast(crash, Monitor) -> no:exist(), {noreply, Monitor}.

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

handle_call(getMonitor,_From, Monitor)      -> {reply, Monitor, Monitor};
handle_call(terminate,_From,Monitor) -> {stop, normal, ok, Monitor}.

terminate(normal, _) -> io:format("Terminating server. Bye~n",[]), ok.
