-module(pollution3).
-author("Agnieszka Dutka").
-record(station, {coordinates, name, measurements=[]}).
-record(measurement, {measure_station, datetime, m_type}).
%% API
-export([createMonitor/0, addStation/3]).

createMonitor() -> [].

addStation(Stations, Name, Coord)->
  case inStations(Name, Coord, Stations) of
    false -> [#station{coordinates = Coord, name=Name} |Stations];
    true -> error
  end.

inStations(_,_, []) -> false;
inStations(Name, Coord, [#station{name = Sname, coordinates = SCoord} |_]) when Name==Sname;SCoord==Coord-> true;
inStations(Name, Coord, [_|T]) -> inStations(Name, Coord, T).



