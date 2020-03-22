-module(pollution3).
-author("Agnieszka Dutka").
-record(station, {coordinates, name, measurements=[]}).
-record(measurement, {measure_station, datetime, m_type}).
-define(IS_COORD(Geo), is_tuple(Geo), size(Geo)==2, is_number(element(1, Geo)), is_number(element(2, Geo))).
-define(IS_DATE(Date), is_tuple(Date), size(Date)==3, is_integer(element(1, Date)), is_integer(element(2, Date)), is_integer(element(3, Date))).
-define(IS_TIME(Time), is_tuple(Time), size(Time)==3, is_integer(element(1, Time)), is_integer(element(2, Time)), is_integer(element(3, Time))).
-define(IS_DATE_TIME(Datetime), is_tuple(Datetime), size(Datetime)==2, ?IS_DATE(element(1, Datetime)), ?IS_TIME(element(2, Datetime))).
%% API
-export([createMonitor/0, addStation/3]).

createMonitor() -> [].

addStation(Stations, Name, Coord)->
  case inStations(Name, Coord, Stations) of
    false -> [#station{coordinates = Coord, name=Name} |Stations];
    true -> throw("station already exists")
  end.

inStations(_,_, []) -> false;
inStations(Name, Coord, [#station{name = Sname, coordinates = SCoord} |_]) when Name==Sname;SCoord==Coord-> true;
inStations(Name, Coord, [_|T]) -> inStations(Name, Coord, T).



