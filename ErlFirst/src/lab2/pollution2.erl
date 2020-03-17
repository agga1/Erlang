-module(pollution2).
-author("Agnieszka Dutka").
-record(station, {coordinates, name}).
-record(measurement, {datetime, m_type}).
-record(monitor, {stations}).

%% API
-export([createMonitor/0]).
createMonitor() ->
  #monitor{stations = dict:new()}.
%%addStation(Monitor, Name, Coord) ->
%%  FoundName = dict:is_key(#station{name=Name, coordinates = Coord},Monitor#monitor.stations),
%%  FoundCoord =  dict:is_key(#station{name=_, coordinates = Coord},Monitor#monitor.stations),
%%  case {FoundName, FoundCoord} of
%%    {false, false} -> #monitor{stations = dict:append(#station{coordinates = Coord, name = Name}, [], Monitor#monitor.stations)};
%%    {_, _} -> error
%%  end.

