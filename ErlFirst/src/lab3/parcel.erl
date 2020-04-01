-module(parcel).
-author("Agnieszka Dutka").

%% API
-export([randList/1, findMyParcelLocker/2, findLockersSeq/2, findLoop/3, findLockersParallel/2]).

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
  lists:foreach(fun(P)-> spawn(?MODULE, findLoop, [self(), P, Lockers]) end,Persons),
  gatherResults([], length(Persons)).

gatherResults(Results, 0)-> Results;
gatherResults(Results, Rest)->
  receive
    Res -> gatherResults([Res|Results], Rest-1)
  end.

findLoop(PID, Person, Lockers)->
  PID ! {Person, findMyParcelLocker(Person, Lockers)}.
