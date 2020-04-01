-module(parcel).
-author("Agnieszka Dutka").

%% API
-export([randList/1, findMyParcelLocker/2, findLockersSeq/2, findLoop/3, findLockersParallel/2, findSingle/3, findLockersSemiParallel/3]).

%% Data
randList(N) -> [ {random:uniform(10001)-1,random:uniform(10001)-1}  || _<-lists:seq(1, N)].

%% findMyParcelLocker(PersonLocation, LockerLocations)
findMyParcelLocker(_, []) -> {error, no_lockers};
findMyParcelLocker({XP, YP}, [H|Lockers]) ->
  DistSq = fun({X, Y}) -> math:pow(XP-X, 2)+ math:pow(YP-Y, 2) end,
  lists:foldl(fun(L, Acc) ->
              case DistSq(L)<DistSq(Acc) of
                true -> L;
                false -> Acc
              end end, H, Lockers).

findLockersSeq(_, [])->  {error, no_lockers};
findLockersSeq(Persons, Lockers)->
    lists:map(fun(P) -> {P, findMyParcelLocker(P, Lockers)} end, Persons).

findLockersParallel(_, []) ->  {error, no_lockers};
findLockersParallel(Persons, Lockers) ->
  lists:foreach(fun(P)-> spawn(?MODULE, findSingle, [self(), P, Lockers]) end,Persons),
  gatherResult([], length(Persons)).

gatherResult(Results, 0)-> Results;
gatherResult(Results, Rest)->
  receive
    Res -> gatherResult([Res|Results], Rest-1)
  end.

findSingle(PID, Person, Lockers)->
  PID ! {Person, findMyParcelLocker(Person, Lockers)}.

findLockersSemiParallel(_, [], _) ->  {error, no_lockers};
findLockersSemiParallel(Persons, Lockers, Cores) ->
  LP = length(Persons),
  NthStart = fun(I) -> (LP*(I-1) div Cores) + 1 end,
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

findLoop(PID, Persons, Lockers)->
  PID ! findLockersSeq(Persons, Lockers).
