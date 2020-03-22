-module(pollutionAlt).
-author("Agnieszka Dutka").
-record(station, {coords, name, measurements =[]}).
-record(measurement, {datetime, type, value}).
-define(IS_COORD(Geo), is_tuple(Geo), size(Geo)==2, is_number(element(1, Geo)), is_number(element(2, Geo))).
-define(IS_NAME(Name), is_list(Name)).
-define(IS_DATE(Date), is_tuple(Date), size(Date)==3, is_integer(element(1, Date)), is_integer(element(2, Date)), is_integer(element(3, Date))).
-define(IS_TIME(Time), is_tuple(Time), size(Time)==3, is_integer(element(1, Time)), is_integer(element(2, Time)), is_integer(element(3, Time))).
-define(IS_DATE_TIME(Datetime), is_tuple(Datetime), size(Datetime)==2, ?IS_DATE(element(1, Datetime)), ?IS_TIME(element(2, Datetime))).
%% API
-export([createMonitor/0, addStation/3, addValueToStation/4, addValue/5, removeValue/4]).

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
addValueToStation(Date, Type, Value, Station = #station{measurements = Ms}) ->
  Found = fun (#measurement{datetime= FDate, type= FType}) -> (FDate == Date) and (FType == Type) end,
  case lists:any(Found, Ms) of
    false -> Station#station{measurements = [#measurement{datetime = Date, type = Type, value = Value}|Ms]};
    _ -> throw("measurement already exists")
  end.

%% type check
addValue(_, Id,  _, _, _) when not ?IS_NAME(Id) and not ?IS_COORD(Id) -> throw("identifier not correct");
addValue(_, _, Date, _, _) when not ?IS_DATE_TIME(Date) -> throw("date not correct");
addValue(_, _, _, Type, _) when not is_list(Type) -> throw("type not correct");
%% recursion
addValue( [], _, _, _, _) -> throw("station not found");
addValue( [ St = #station{name= Name} | Stations ], Name, Date, Type, Value) -> [ addValueToStation(Date, Type, Value, St) | Stations ];
addValue( [ St = #station{coords= Coords} | Stations ],Coords, Date, Type, Value) -> [ addValueToStation(Date, Type, Value, St) | Stations ];
addValue( [ H | Stations ], Id, Date, Type, Value) -> [H | addValue(Stations, Id, Date, Type, Value)].

%% REMOVING MEASUREMENT -----------------------------------------------------------------------
removeValue([], _, _, _) -> throw("station not found");
removeValue([ St = #station{name= Name, measurements = Ms} | Stations ], Name, Date, Type) ->
  [St#station{measurements = removeValueFromMs(Ms, Date, Type)} | Stations ];
removeValue([ St = #station{coords = Coord, measurements = Ms} | Stations ], Coord, Date, Type) ->
  [St#station{measurements = removeValueFromMs(Ms, Date, Type)} | Stations ];
removeValue([H|Stations], Id, Date, Type) -> [H|removeValue(Stations, Id, Date, Type)].

removeValueFromMs([], _, _) -> throw("measurement to remove not found"); % or just [] ?
removeValueFromMs([#measurement{datetime = Date, type = Type} |Ms], Date, Type) -> Ms;
removeValueFromMs([M|Ms], Date, Type) -> [M| removeValueFromMs(Ms, Date, Type)].

getOneValue([], _, _, _)  -> throw("station not found");
getOneValue([#station{name= Name, measurements = Ms} | _ ], Name, Date, Type)  -> getOneValueFromMs(Ms, Date, Type);
getOneValue([#station{coords = Coord, measurements = Ms} | _ ], Coord, Date, Type)  -> getOneValueFromMs(Ms, Date, Type);
getOneValue([H|Stations], Id, Date, Type) -> getOneValue(Stations, Id, Date, Type).

findStation(Stations, Name) ->




