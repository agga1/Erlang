-module(pollution_server_tests).
-author("Agnieszka Dutka").

%% API
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

start_test() ->
  pollution_server:start(),
  ?assert(lists:member(pollution_server, registered())),
  ?assertEqual([], pollution_server:getMonitor()).

stop_test() ->
  pollution_server:stop(),
  timer:sleep(50),
  ?assert(not lists:member(pollution_server, registered())).

% integrated tests
stations_test_() ->
  {setup,
    fun() -> pollution_server:start() end,
    fun(_) -> pollution_server:stop() end,
    [?_test(addStation()),
      ?_test(addValue()),
      ?_test(getOneValue()),
      ?_test(removeValue()),
      ?_test(getStationMean()),
      ?_test(getDaileMean()),
      ?_test(getPeakHours()),
      ?_test(mostActiveStation())
      ]
  }.

addStation() ->
  ?assertEqual(ok,pollution_server:addStation("one", {1,1})),
  ?assertEqual(ok,pollution_server:addStation("two", {1,2})),
  ?assertMatch({error, _},pollution_server:addStation("one", {1,2})), % duplicate name
  ?assertMatch({error, _},pollution_server:addStation("diff", {1,1})). % duplicate coords

addValue() ->
  ?assertEqual(ok, pollution_server:addValue("one", {{20, 13, 2012}, {18, 0, 0}}, "PM10", 30)), %
  ?assertEqual(ok, pollution_server:addValue("one", {{20, 15, 2012}, {20, 30, 0}}, "PM10", 40)),
  ?assertEqual(ok, pollution_server:addValue("two", {{20, 13, 2012}, {12, 0, 0}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:addValue("two", {{20, 15, 2012}, {16, 0, 0}}, "PM10", 25)),
  ?assertMatch({error, _}, pollution_server:addValue("one", {{20, 13, 2012}, {18, 0, 0}}, "PM10", 10)), % measurement already exists
  ?assertMatch({error, _}, pollution_server:addValue("nonexistent", {{20, 13, 2012}, {18, 0, 0}}, "PM10", 10)). % nonexistent station

getOneValue() ->
  ?assertEqual(30, pollution_server:getOneValue("one", {{20, 13, 2012}, {18, 0, 0}}, "PM10")),
  ?assertEqual(10, pollution_server:getOneValue("two", {{20, 13, 2012}, {12, 0, 0}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:getOneValue("three", {{20, 13, 2012}, {19, 0, 0}}, "PM10")), % nonexistent station
  %% -- pollution module changed to not throw exception (and instead {error, ...}) while extracting nonexistent value !!
  ?assertMatch({error, _}, pollution_server:getOneValue("two", {{20, 13, 2012}, {100, 0, 0}}, "PM10")). % nonexistent value

removeValue() ->
  ?assertEqual(ok, pollution_server:removeValue("one", {{20, 13, 2012}, {18, 0, 0}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:getOneValue("one", {{20, 13, 2012}, {18, 0, 0}}, "PM10")), % measurement shouldn't be found
  ?assertEqual(ok, pollution_server:removeValue("one", {{20, 13, 2012}, {18, 0, 0}}, "PM10")). % quiet remove (nothing happens if value doesn't exist)

getStationMean() ->
  ?assertEqual(17.5 , pollution_server:getStationMean("two", "PM10")), % from 10, 25
  ?assertEqual(0.0 , pollution_server:getStationMean("two", "PM11")), % no measurements will match
  ?assertEqual(40.0 , pollution_server:getStationMean("one", "PM10")). % from 40

getDaileMean() ->
  ?assertEqual(10.0 , pollution_server:getDailyMean({20, 13, 2012}, "PM10")), % from 10
  ?assertEqual(32.5 , pollution_server:getDailyMean({20, 15, 2012}, "PM10")), % from 40, 25
  ?assertEqual(0.0 , pollution_server:getDailyMean({20, 15, 2012}, "PM11")). % no stations will match

getPeakHours() ->
  ?assertEqual({40.0, ["20"]}, pollution_server:getPeakHours("PM10")),
  ?assertMatch({0.0, _}, pollution_server:getPeakHours("PM11")).

mostActiveStation() ->
  ?assertEqual(["two"], pollution_server:mostActiveStation()). % "two" has 2 measuerements, "one" - 1






