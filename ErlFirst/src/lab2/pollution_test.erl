-module(pollution_test).
-author("Agnieszka Dutka").

%% API
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

addStation_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addStation(M1,"TWO", {1, 2}),
  ?assertEqual(3, length(pollution:addStation(M2, "THREE", {3, 1}))),
  ?assertMatch({error, illegal_name}, pollution:addStation(M2, 3, {4, 1})),
  ?assertMatch({error, station_exists}, pollution:addStation(M2, "ONE", {2,3})),
  ?assertMatch({error, station_exists}, pollution:addStation(M2, "FOUR", {1,1})).
