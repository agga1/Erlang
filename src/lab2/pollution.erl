-module(pollution).
-author("Agnieszka Dutka").
-record(station, {coords, name, measurements =#{}}).
-record(mkey, {datetime, type}).
-define(IS_COORD(Geo), is_tuple(Geo), size(Geo)==2, is_number(element(1, Geo)), is_number(element(2, Geo))).
-define(IS_NAME(Name), is_list(Name)).
-define(IS_DATE(Date), is_tuple(Date), size(Date)==3, is_integer(element(1, Date)), is_integer(element(2, Date)), is_integer(element(3, Date))).
-define(IS_TIME(Time), is_tuple(Time), size(Time)==3, is_integer(element(1, Time)), is_integer(element(2, Time)), is_integer(element(3, Time))).
-define(IS_DATE_TIME(Datetime), is_tuple(Datetime), size(Datetime)==2, ?IS_DATE(element(1, Datetime)), ?IS_TIME(element(2, Datetime))).
%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getPeakHours/2, getHourlyMean/3, mostActiveStation/1]).

createMonitor() -> [].

addStation(_, Name, _) when not ?IS_NAME(Name) -> {error, illegal_name};
addStation(_, _, Coord) when not ?IS_COORD(Coord) ->{error, illegal_coordinates};
addStation(Stations, Name, Coord)->
  Found = fun (#station{name = Name1, coords = Coord1}) -> (Name1 == Name) or  (Coord1 == Coord) end,
  case lists:any(Found, Stations) of
    false ->[#station{coords = Coord, name=Name} |Stations];
    _ -> {error, station_exists}
  end.

%% ADDING MEASUREMENT -----------------------------------------------------------------------
% adding measurement to list of measurements Ms
addMeasurement(_, Date, Type,_)  when not ?IS_DATE_TIME(Date);  not is_list(Type) -> throw("incorrect measurement data");
addMeasurement(Ms, Date, Type,Value) ->
  Found = maps:is_key(#mkey{datetime = Date, type = Type}, Ms),
  case Found of
    true -> {error, measurement_exists};
    _ -> maps:put(#mkey{datetime = Date, type = Type}, Value, Ms)
  end.
% searching for station with given Id and modifying its measurements map
addValue(_, Id,  _, _, _) when not ?IS_NAME(Id) and not ?IS_COORD(Id) -> {error, incorrect_id};
addValue( [], _, _, _, _) -> {error, station_not_found};
addValue( [ St = #station{name= Name, measurements = Ms} | Stations ], Name, Date, Type, Value) ->
  [ St#station{measurements = addMeasurement(Ms, Date, Type, Value)} | Stations ];
addValue( [ St = #station{coords= Coords, measurements = Ms} | Stations ],Coords, Date, Type, Value) ->
  [ St#station{measurements = addMeasurement(Ms, Date, Type, Value)} | Stations ];
addValue( [ H | Stations ], Id, Date, Type, Value) -> [H | addValue(Stations, Id, Date, Type, Value)].

%% REMOVING MEASUREMENT -----------------------------------------------------------------------
removeValue([], _, _, _) -> {error, station_not_found};
removeValue([ St = #station{name= Name, measurements = Ms} | Stations ], Name, Date, Type) ->
  [St#station{measurements = maps:remove(#mkey{datetime = Date, type = Type}, Ms)} | Stations ];
removeValue([ St = #station{coords = Coord, measurements = Ms} | Stations ], Coord, Date, Type) ->
  [St#station{measurements = maps:remove(#mkey{datetime = Date, type = Type}, Ms)} | Stations ];
removeValue([H|Stations], Id, Date, Type) -> [H|removeValue(Stations, Id, Date, Type)].

%% GET ONE VALUE ------------------------------------------------------------------------------
getOneValue([], _, _, _)  -> {error, station_not_found};
getOneValue([#station{name= Name, measurements = Ms} | _ ], Name, Date, Type)  ->
  maps:get(#mkey{datetime = Date, type = Type}, Ms);
getOneValue([#station{coords = Coord, measurements = Ms} | _ ], Coord, Date, Type)  ->
  maps:get(#mkey{datetime = Date, type = Type}, Ms);
getOneValue([_|Stations], Id, Date, Type) -> getOneValue(Stations, Id, Date, Type).
%% MEAN --------------------------------------------------------------------------------------
calculateMean(_, 0) -> 0;  % exclude division by 0
calculateMean(Sum, N) -> Sum/N.

%% STATION MEAN ------------------------------------------------------------------------------
getStationMean([], _, _) -> {error, station_not_found};
getStationMean([#station{name= Name, measurements = Ms} | _ ], Name, Type) -> getMean(Ms, Type);
getStationMean([#station{coords= Coord, measurements = Ms} | _ ], Coord, Type) -> getMean(Ms, Type);
getStationMean([_ | Stations ], Id, Type) -> getStationMean(Stations, Id, Type).

getMean(Ms, Type) ->
  Filtered = maps:filter(fun(#mkey{type = T}, _) -> T==Type end, Ms),
  Sum = maps:fold( fun(_, V, Acc) -> V+Acc end, 0, Filtered),
  calculateMean(Sum, maps:size(Filtered)).

%% DAILY MEAN ------------------------------------------------------------------------------
getDailyMean(Stations, Day, Type) ->
  ToFilteredValues = fun(#station{measurements = Ms}, Day, Type) ->
    maps:values( maps:filter(fun(#mkey{type = T, datetime = {FDay, _}}, _) -> T==Type andalso FDay==Day end, Ms) )
    end,
  Values = lists:foldl(fun(St, Acc)-> ToFilteredValues(St, Day, Type)++Acc end ,[], Stations),
  calculateMean(lists:sum(Values), length(Values)).

%% HOUR OF HIGHEST PEAK -------
% find hours in which average value for given type of measurement across stations is the highest
getPeakHours(Stations, Type) ->
  AvgForHour = [ {Hour, getHourlyMean(Stations, Hour, Type)} || Hour<-lists:seq(0, 23)],
  MaxVal = lists:foldl(fun({_, Val}, Acc)-> max(Val, Acc) end ,0,AvgForHour),
  AllHighest = lists:filter(fun({_, Val})->Val==MaxVal end,AvgForHour),
  Hours = lists:map(fun({Hour, _})-> integer_to_list(Hour) end, AllHighest),
  io:format("highest value: ~p~npeak hours: ~p~n", [MaxVal,Hours]).

getHourlyMean(Stations, Hour, Type) ->
  ToFilteredValues = fun(#station{measurements = Ms}, Hour, Type) ->
    maps:values( maps:filter(fun(#mkey{type = T, datetime = {_, {FHour, _, _}}}, _) -> T==Type andalso FHour==Hour end, Ms) )
           end,
  Values = lists:foldl(fun(St, Acc)-> ToFilteredValues(St, Hour, Type)++Acc end ,[], Stations),
  calculateMean(lists:sum(Values), length(Values)).

%% MOST ACTIVE STATION -------------------------------------------------------------------------
% displays name of station which registered the most measurements
mostActiveStation(Stations) ->
  MsForEach = lists:map(fun(#station{name=Name, measurements = Ms})-> {Name, maps:size(Ms)} end , Stations),
  MaxMs = lists:foldl(fun({_, Val}, Acc)-> max(Val, Acc) end ,0,MsForEach),
  MostActive =  lists:filter(fun({_, Val})->Val==MaxMs end,MsForEach),
  io:format("Most active stations: ~p~n", [lists:map(fun({Name, _})-> Name end, MostActive)]).
