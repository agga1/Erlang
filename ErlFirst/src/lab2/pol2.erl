-module(pol2).
-author("Agnieszka Dutka").
-record(station, {coords, name, measurements =#{}}).
-record(mkey, {datetime, type}).
-define(IS_COORD(Geo), is_tuple(Geo), size(Geo)==2, is_number(element(1, Geo)), is_number(element(2, Geo))).
-define(IS_NAME(Name), is_list(Name)).
-define(IS_DATE(Date), is_tuple(Date), size(Date)==3, is_integer(element(1, Date)), is_integer(element(2, Date)), is_integer(element(3, Date))).
-define(IS_TIME(Time), is_tuple(Time), size(Time)==3, is_integer(element(1, Time)), is_integer(element(2, Time)), is_integer(element(3, Time))).
-define(IS_DATE_TIME(Datetime), is_tuple(Datetime), size(Datetime)==2, ?IS_DATE(element(1, Datetime)), ?IS_TIME(element(2, Datetime))).
%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3]).

createMonitor() -> [].

addStation(_, Name, _) when not ?IS_NAME(Name) -> throw("provided name is illegal");
addStation(_, _, Coord) when not ?IS_COORD(Coord) -> throw("geographical coordinates incorrect");
addStation(Stations, Name, Coord)->
  Found = fun (#station{name = Name1, coords = Coord1}) -> (Name1 == Name) or  (Coord1 == Coord) end,
  case lists:any(Found, Stations) of
    false ->[#station{coords = Coord, name=Name} |Stations];
    _ -> throw("station already exists")
  end.

%% ADDING MEASUREMENT -----------------------------------------------------------------------
addMeasurement(_, Date, Type,_)  when not ?IS_DATE_TIME(Date);  not is_list(Type) -> throw("incorrect measurement data");
addMeasurement(Ms, Date, Type,Value) ->
  Found = maps:is_key(#mkey{datetime = Date, type = Type}, Ms),
  case Found of
    true -> throw("measurement already exist");
    _ -> maps:put(#mkey{datetime = Date, type = Type}, Value, Ms)
  end.

addValue(_, Id,  _, _, _) when not ?IS_NAME(Id) and not ?IS_COORD(Id) -> throw("identifier not correct");
addValue( [], _, _, _, _) -> throw("station not found");
addValue( [ St = #station{name= Name, measurements = Ms} | Stations ], Name, Date, Type, Value) ->
  [ St#station{measurements = addMeasurement(Ms, Date, Type, Value)} | Stations ];
addValue( [ St = #station{coords= Coords, measurements = Ms} | Stations ],Coords, Date, Type, Value) ->
  [ St#station{measurements = addMeasurement(Ms, Date, Type, Value)} | Stations ];
addValue( [ H | Stations ], Id, Date, Type, Value) -> [H | addValue(Stations, Id, Date, Type, Value)].

%% REMOVING MEASUREMENT -----------------------------------------------------------------------
removeValue([], _, _, _) -> throw("station not found");
removeValue([ St = #station{name= Name, measurements = Ms} | Stations ], Name, Date, Type) ->
  [St#station{measurements = maps:remove(#mkey{datetime = Date, type = Type}, Ms)} | Stations ];
removeValue([ St = #station{coords = Coord, measurements = Ms} | Stations ], Coord, Date, Type) ->
  [St#station{measurements = maps:remove(#mkey{datetime = Date, type = Type}, Ms)} | Stations ];
removeValue([H|Stations], Id, Date, Type) -> [H|removeValue(Stations, Id, Date, Type)].

%% GET ONE VALUE ------------------------------------------------------------------------------
getOneValue([], _, _, _)  -> throw("station not found");
getOneValue([#station{name= Name, measurements = Ms} | _ ], Name, Date, Type)  ->
  maps:get(#mkey{datetime = Date, type = Type}, Ms);
getOneValue([#station{coords = Coord, measurements = Ms} | _ ], Coord, Date, Type)  ->
  maps:get(#mkey{datetime = Date, type = Type}, Ms);
getOneValue([_|Stations], Id, Date, Type) -> getOneValue(Stations, Id, Date, Type).

%% STATION MEAN ------------------------------------------------------------------------------
getStationMean([], _, _) -> throw("station not found");
getStationMean([#station{name= Name, measurements = Ms} | _ ], Name, Type) -> getMean(Ms, Type);
getStationMean([#station{coords= Coord, measurements = Ms} | _ ], Coord, Type) -> getMean(Ms, Type);
getStationMean([_ | Stations ], Id, Type) -> getStationMean(Stations, Id, Type).

getMean(Ms, Type) ->
  Filtered = maps:filter(fun(#mkey{type = T}, _) -> T==Type end, Ms),
  Sum = maps:fold( fun(_, V, Acc) -> V+Acc end, 0, Filtered),
  Sum/maps:size(Filtered).

%% DAILY MEAN ------------------------------------------------------------------------------





