-module(parcel).
-author("Agnieszka Dutka").
-export([randList/1, findMyParcelLocker/2, findLockersSeq/2, findLoop/3, findLockersParallel/2, findSingle/3, findLockersSemiParallel/3,  compareTime/2]).

randList(N) -> [ {random:uniform(10001)-1,random:uniform(10001)-1}  || _<-lists:seq(1, N)].

getTimeinSec(Func, Args) ->
  {Time,_} = timer:tc(?MODULE, Func, Args),
  Time/1000000.

compareTime(PersonsNo, LockersNo) ->
  Persons = randList(PersonsNo),
  Lockers =  randList(LockersNo),
  Time1 = getTimeinSec(findLockersSeq,[Persons, Lockers]),
  Time2 = getTimeinSec(findLockersParallel, [Persons, Lockers]),
  Time3 = getTimeinSec(findLockersSemiParallel, [Persons, Lockers, 8]),
  io:format("Elapsed time:~nsequntial: ~ps~nparallel: ~ps~nsemi-parallel: ~ps~n", [Time1, Time2, Time3]).

%% -------------findMyParcelLocker(PersonLocation, LockerLocations)
findMyParcelLocker(_, []) -> {error, no_lockers};
findMyParcelLocker({XP, YP}, [H|Lockers]) ->
  DistSq = fun({X, Y}) -> math:pow(XP-X, 2)+ math:pow(YP-Y, 2) end,
  lists:foldl(fun(L, Acc) ->
              case DistSq(L)<DistSq(Acc) of
                true -> L;
                false -> Acc
              end end, H, Lockers).

%% -------------sequentially find locker for each person
findLockersSeq(_, [])->  {error, no_lockers};
findLockersSeq(Persons, Lockers)->
    lists:map(fun(P) -> {P, findMyParcelLocker(P, Lockers)} end, Persons).

%% -------------Parallel version - new process for every person
findLockersParallel(_, []) ->  {error, no_lockers};
findLockersParallel(Persons, Lockers) ->
  lists:foreach(fun(P)-> spawn(?MODULE, findSingle, [self(), P, Lockers]) end,Persons),
  gatherResult([], length(Persons)).

gatherResult(Results, 0)-> Results;  % written separately for better performance
gatherResult(Results, Rest)->
  receive
    Res -> gatherResult([Res|Results], Rest-1)
  end.

findSingle(PID, Person, Lockers)-> PID ! {Person, findMyParcelLocker(Person, Lockers)}.

%% -------------Semi parallel version - new process for every Core
findLockersSemiParallel(_, [], _) ->  {error, no_lockers};
findLockersSemiParallel(Persons, Lockers, Cores) ->
  NthStart = fun(I) -> (length(Persons)*(I-1) div Cores) + 1 end,
  NthLen = fun(I) -> NthStart(I+1)- NthStart(I) end,
  lists:foreach(fun(CoreNr)->
    spawn(?MODULE, findLoop, [self(), lists:sublist(Persons, NthStart(CoreNr),NthLen(CoreNr) ), Lockers])
                end, lists:seq(1, Cores)),
  gatherResults([], Cores).

gatherResults(Results, 0)-> Results;
gatherResults(Results, Rest)->
  receive
    Res -> gatherResults(Res++Results, Rest-1)
  end.

findLoop(PID, Persons, Lockers)-> PID ! findLockersSeq(Persons, Lockers).
