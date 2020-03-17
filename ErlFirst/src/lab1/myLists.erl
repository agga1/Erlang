%%%-------------------------------------------------------------------
%%% @author Agnieszka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 11:59
%%%-------------------------------------------------------------------
-module(myLists).
-author("Agnieszka").

%% API
-export([contains/2, duplicElem/1, sumFloats/1]).

contains([], _) -> false;
contains([X|_], X) -> true;
contains([_|T], X) -> contains(T, X).

duplicElem([]) -> [];
%%duplicElem([H|T]) -> [H, H]++duplicElem(T).
duplicElem([H|T]) -> [H, H |duplicElem(T)].


sumFloats([]) -> 0.0;
sumFloats([H|T]) when is_float(H) -> H+sumFloats(T);
sumFloats([_|T]) -> sumFloats(T).