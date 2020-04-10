-module(pollution).
-author("Agnieszka Dutka").
-record(station, {coords, name, measurements =#{}}).
-record(measKey, {datetime, type}).
-define(IS_NAME(Name), is_list(Name)).
-define(IS_COORD(Geo), (is_tuple(Geo) andalso  size(Geo)==2 andalso  is_number(element(1, Geo)) andalso  is_number(element(2, Geo)))).
-define(IS_DATE(Date), (is_tuple(Date) andalso size(Date)==3 andalso is_integer(element(1, Date)) andalso is_integer(element(2, Date)) andalso  is_integer(element(3, Date)))).
-define(IS_TIME(Time), (is_tuple(Time) andalso size(Time)==3 andalso is_integer(element(1, Time)) andalso is_integer(element(2, Time)) andalso is_integer(element(3, Time)))).
-define(IS_DATE_TIME(Datetime), (is_tuple(Datetime) andalso size(Datetime)==2 andalso ?IS_DATE(element(1, Datetime)) andalso ?IS_TIME(element(2, Datetime)))).
%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getPeakHours/2, getHourlyMean/3, mostActiveStation/1]).
%%
createMonitor() -> [].
%% ADDING STATION -----------------------------------------------------------------------
%% arguments:    (Monitor, Station Name, Station Coordinates)   returns: Monitor with added station
addStation(_, Name, _) when not ?IS_NAME(Name) -> {error, illegal_name};
addStation(_, _, Coord) when not ?IS_COORD(Coord) ->{error, illegal_coordinates};
addStation(Stations, Name, Coord)->
  Found = fun (#station{name = Name1, coords = Coord1}) -> (Name1 == Name) or  (Coord1 == Coord) end,
  case lists:any(Found, Stations) of
    false ->[#station{coords = Coord, name=Name} |Stations];
    _ -> {error, station_exists}
  end.

%% ADDING MEASUREMENT -----------------------------------------------------------------------
% adding measurement to given list of measurements Ms
addMeasurement(_, Date, Type,_)  when not ?IS_DATE_TIME(Date);  not is_list(Type) -> {error, incorrect_datatype};
addMeasurement(Ms, Date, Type,Value) ->
  Found = maps:is_key(#measKey{datetime = Date, type = Type}, Ms),
  case Found of
    true -> {error, measurement_exists};
    _ -> maps:put(#measKey{datetime = Date, type = Type}, Value, Ms)
  end.
% searching for station with given Id and modifying its measurements map
%% arguments:    addValue(Monitor, StationId (Name or Coordinates),  Date, Type, Value),
%% returns:      modified Monitor (or tuple {error, error_message} )
addValue(_, Id,  _, _, _) when not ?IS_NAME(Id) and not ?IS_COORD(Id) -> {error, incorrect_id};
addValue( [], _, _, _, _) -> {error, station_not_found};
addValue( [ St = #station{name= Name, measurements = Ms} | Stations ], Name, Date, Type, Value) ->
  NewMeasurements = addMeasurement(Ms, Date, Type, Value),
  case NewMeasurements of
    {error, Msg} -> {error, Msg};
     _           -> [ St#station{measurements = NewMeasurements} | Stations ]
  end;
addValue( [ St = #station{coords= Coords, measurements = Ms} | Stations ],Coords, Date, Type, Value) ->
  NewMeasurements = addMeasurement(Ms, Date, Type, Value),
  case NewMeasurements of
    {error, Msg} -> {error, Msg};
    _           -> [ St#station{measurements = NewMeasurements} | Stations ]
  end;
addValue( [ H | Stations ], Id, Date, Type, Value) ->
  Rest = addValue(Stations, Id, Date, Type, Value),
  case Rest of
    {error, Msg} -> {error, Msg};
    _           -> [ H | Rest ]
  end.

%% REMOVING MEASUREMENT -----------------------------------------------------------------------
% removes measurement with given Date and Type from station
% arguments: (Monitor, StationId, Date, Type), returns: modified Monitor
removeValue([], _, _, _) -> [];  % quiet remove
removeValue([ St = #station{name= Name, measurements = Ms} | Stations ], Name, Date, Type) ->
  [St#station{measurements = maps:remove(#measKey{datetime = Date, type = Type}, Ms)} | Stations ];
removeValue([ St = #station{coords = Coord, measurements = Ms} | Stations ], Coord, Date, Type) ->
  [St#station{measurements = maps:remove(#measKey{datetime = Date, type = Type}, Ms)} | Stations ];
removeValue([H|Stations], Id, Date, Type) -> [H|removeValue(Stations, Id, Date, Type)].

%% GET ONE VALUE ------------------------------------------------------------------------------
% returns:   value of measurement with given Date and Type
% arguments: (Monitor, StationId, Date, Type)
getOneValue([], _, _, _)  -> {error, station_not_found};
getOneValue([#station{name= Name, measurements = Ms} | _ ], Name, Date, Type)  ->
  maps:get(#measKey{datetime = Date, type = Type}, Ms, {error, no_such_value});
getOneValue([#station{coords = Coord, measurements = Ms} | _ ], Coord, Date, Type)  ->
  maps:get(#measKey{datetime = Date, type = Type}, Ms, {error, no_such_value});
getOneValue([_|Stations], Id, Date, Type) -> getOneValue(Stations, Id, Date, Type).

%% MEAN --------------------------------------------------------------------------------------
calculateMean(_, 0) -> 0.0;  % exclude division by 0
calculateMean(Sum, N) -> Sum/N.

%% STATION MEAN ------------------------------------------------------------------------------
% arguments: (Monitor, StationId, Type)
% returns: mean value of all measurements of a given type for a given station
getStationMean([], _, _) -> {error, station_not_found};
getStationMean([#station{name= Name, measurements = Ms} | _ ], Name, Type) -> getMean(Ms, Type);
getStationMean([#station{coords= Coord, measurements = Ms} | _ ], Coord, Type) -> getMean(Ms, Type);
getStationMean([_ | Stations ], Id, Type) -> getStationMean(Stations, Id, Type).

getMean(Ms, Type) ->
  Filtered = maps:filter(fun(#measKey{type = T}, _) -> T==Type end, Ms),
  Sum = maps:fold( fun(_, V, Acc) -> V+Acc end, 0, Filtered),
  calculateMean(Sum, maps:size(Filtered)).

%% DAILY MEAN ------------------------------------------------------------------------------
% returns mean value of all measurements of a given type on a given day
% arguments: (Monitor, Day, Type)
getDailyMean(Stations, Day, Type) ->
  ToFilteredValues = fun(#station{measurements = Ms}, Day, Type) ->
    maps:values( maps:filter(fun(#measKey{type = T, datetime = {FDay, _}}, _) -> T==Type andalso FDay==Day end, Ms) )
    end,
  Values = lists:foldl(fun(St, Acc)-> ToFilteredValues(St, Day, Type)++Acc end ,[], Stations),
  calculateMean(lists:sum(Values), length(Values)).

%% HOUR OF HIGHEST PEAK -------
% find hours in which average value for given type of measurement across stations is the highest
% returns: tuple {highest average for an hour , hours in which it occurred }
getPeakHours(Stations, Type) ->
  AvgForHour = [ {Hour, getHourlyMean(Stations, Hour, Type)} || Hour<-lists:seq(0, 23)],
  MaxVal = lists:foldl(fun({_, Val}, Acc)-> max(Val, Acc) end ,0,AvgForHour),
  AllHighest = lists:filter(fun({_, Val})->Val==MaxVal end,AvgForHour),
  Hours = lists:map(fun({Hour, _})-> integer_to_list(Hour) end, AllHighest),
  io:format("highest value: ~p~npeak hours: ~p~n", [MaxVal,Hours]),
  {MaxVal, Hours}.

% get mean across stations for given hour of given measurement type
getHourlyMean(Stations, Hour, Type) ->
  ToFilteredValues = fun(#station{measurements = Ms}, Hour, Type) ->
    maps:values( maps:filter(fun(#measKey{type = T, datetime = {_, {FHour, _, _}}}, _) -> T==Type andalso FHour==Hour end, Ms) )
           end,
  Values = lists:foldl(fun(St, Acc)-> ToFilteredValues(St, Hour, Type)++Acc end ,[], Stations),
  calculateMean(lists:sum(Values), length(Values)).

%% MOST ACTIVE STATION -------------------------------------------------------------------------
% displays name of station(s) which registered the most measurements
% returns: names of most active stations
mostActiveStation(Stations) ->
  MsForEach = lists:map(fun(#station{name=Name, measurements = Ms})-> {Name, maps:size(Ms)} end , Stations),
  MaxMs = lists:foldl(fun({_, Val}, Acc)-> max(Val, Acc) end ,0,MsForEach),
  MostActive =  lists:filter(fun({_, Val})->Val==MaxMs end,MsForEach),
  io:format("Most active stations: ~p~n", [lists:map(fun({Name, _})-> Name end, MostActive)]),
  lists:map(fun({Name, _})-> Name end, MostActive).

