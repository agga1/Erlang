-module(qsort).
-author("Agnieszka Dutka").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3]).
lessThan(List, Arg) -> [X || X<-List, X<Arg].
grtEqThan(List, Arg) -> [ X || X<-List, X>=Arg].
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) );
qs([]) ->[].

randomElems(N,Min,Max)-> [ random:uniform(Max-Min+1) + Min-1 || _ <-lists:seq(1, N)].
compareSpeeds(List, Fun1, Fun2) ->
  {T1, _} = timer:tc(Fun1, [List]),
  {T2, _} = timer:tc(Fun2, [List]),
  io:format("first func: ~p~nsecond func:~p~n", [T1, T2]).
