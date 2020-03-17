-module(pollution).
-author("Agnieszka Dutka").
-record(station, {coordinates, name, measurements=[]}).
-record(measurement, {measure_station, datetime, m_type}).
-record(monitor, {stations=[]}).
%% API
-export([createMonitor/0, addStation/3]).

createMonitor() ->
  #monitor{}.
addStation(#monitor{stations = Stations}, Name, Coord)->
  case exists(Name, Coord, Stations) of
    false -> #monitor{stations = [#station{coordinates = Coord, name=Name}|Stations]};
    true -> error
  end.

exists(_,_, []) -> false;
exists(Name, Coord, [#station{name = Sname, coordinates = SCoord} |_]) when Name==Sname;SCoord==Coord-> true;
exists(Name, Coord, [_|T]) -> exists(Name, Coord, T).


