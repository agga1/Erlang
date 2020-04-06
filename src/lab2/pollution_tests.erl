-module(pollution_tests).
-author("Agnieszka Dutka").

%% API
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

create_test() ->
  ?assertEqual(pollution:createMonitor(), []).

addStation_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addStation(M1,"TWO", {1, 2}),
  ?assertEqual(2, length(M2)),   % 2 stations should be added
  ?assertMatch({error, illegal_name}, pollution:addStation(M2, 3, {4, 1})),
  ?assertMatch({error, station_exists}, pollution:addStation(M2, "ONE", {2,3})),
  ?assertMatch({error, station_exists}, pollution:addStation(M2, "THREE", {1, 1})),
  ?assertMatch({error, illegal_coordinates}, pollution:addStation(M2, "THREE", {"2",3})),
  ?assertMatch({error, illegal_coordinates}, pollution:addStation(M2, "THREE", {1, 2,3})).

addValue_test()->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addValue(M1, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10", 50),
  ?assertMatch({error, station_not_found}, pollution:addValue(M1, "TWO", calendar:local_time(), "PM10", 9) ),
  ?assertMatch({error, incorrect_datatype}, pollution:addValue(M1, "ONE", {{1, 2, 3}}, "PM10", 9) ),
  ?assertMatch({error, incorrect_datatype}, pollution:addValue(M1, "ONE", calendar:local_time(), 150, 9) ),
  ?assertMatch({error, measurement_exists}, pollution:addValue(M2, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10", 9) ).

removeValue_test()->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addValue(M1, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10", 50),
  [H|_] = pollution:removeValue(M2, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10"),
  {station, _, _, Ms} = H,
  ?assertEqual(0, maps:size(Ms)),
  M3 = pollution:removeValue(M2, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10"), % quiet remove - nothing happens if not found
  ?assertMatch([{station,{1,1},"ONE",#{}}], M3).

getOneValue_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addStation(M1,"TWO", {1, 2}),
  M3 = pollution:addValue(M2, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10", 50),
  V = pollution:getOneValue(M3, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10"),
  ?assertEqual(V, 50).

getStationMean_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addValue(M1, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10", 50),
  M3 = pollution:addValue(M2, "ONE", {{1, 2, 1}, {1, 1, 1}}, "PM10", 40),
  M4 = pollution:addStation(M3, "TWO", {2, 2}),
  M5 = pollution:addValue(M4, "TWO", {{1, 2, 1}, {1, 1, 1}}, "PM10", 10),
  ?assertEqual(45.0, pollution:getStationMean(M5, "ONE", "PM10")),
  ?assertEqual(0.0, pollution:getStationMean(M5, "ONE", "PM9")).

getDailyMean_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addStation(M1, "TWO", {2, 1}),
  M3 = pollution:addValue(M2, "ONE", {{1, 1, 1}, {1, 1, 1}}, "PM10", 50),
  M4 = pollution:addValue(M3, "TWO", {{1, 1, 1}, {1, 1, 1}}, "PM10", 40),
  ?assertEqual(45.0, pollution:getDailyMean(M4, {1, 1, 1}, "PM10")),
  ?assertEqual(0.0, pollution:getDailyMean(M4, {1, 1, 1}, "PM11")).

getPeakHours_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addStation(M1, "TWO", {2, 1}),
  M3 = pollution:addValue(M2, "ONE", {{1, 1, 1}, {18, 1, 1}}, "PM10", 50),
  M4 = pollution:addValue(M3, "TWO", {{1, 1, 1}, {18, 20, 1}}, "PM10", 40),
  M5 = pollution:addValue(M4, "TWO", {{1, 1, 1}, {19, 30, 1}}, "PM10", 40),
  M6 = pollution:addValue(M5, "TWO", {{1, 1, 1}, {16, 50, 1}}, "PM10", 40),
  {MaxVal, Hours} = pollution:getPeakHours(M6, "PM10"),
  ?assertEqual(MaxVal, 45.0),
  ?assertMatch(Hours, ["18"]).

mostActiveStation_test()->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "ONE", {1, 1}),
  M2 = pollution:addStation(M1, "TWO", {2, 1}),
  M3 = pollution:addValue(M2, "ONE", {{1, 1, 1}, {18, 1, 1}}, "PM10", 50),
  M4 = pollution:addValue(M3, "TWO", {{1, 1, 1}, {18, 1, 1}}, "PM10", 40),
  ?assertEqual(2, length(pollution:mostActiveStation(M4))).
