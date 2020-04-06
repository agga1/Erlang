-module(onp).
-author("Agnieszka Dutka").

%% API
-export([onp/1]).

onpIns([H|_], []) -> H;
onpIns([A, B |T], ["+" | T2]) -> onpIns([B+A | T], T2);
onpIns([A, B |T], ["-" | T2]) -> onpIns([B-A | T], T2);
onpIns([A, B |T], ["*" | T2]) -> onpIns([B*A | T], T2);
onpIns([A, B |T], ["/" | T2]) -> onpIns([B/A | T], T2);
onpIns([A, B |T], ["^" | T2]) -> onpIns([math:pow(B, A) | T], T2);
onpIns([A, B |T], ["@" | T2]) -> onpIns([max(B, A) | T], T2);     %% deletes smaller number
onpIns([A |T], ["$" | T2]) -> onpIns([A*3.94 | T], T2);           %% returns value in dollars (converted from PLN)
onpIns([A |T], ["sqrt" | T2]) -> onpIns([math:sqrt(A) | T], T2);
onpIns([A |T], ["sin" | T2]) -> onpIns([math:sin(A) | T], T2);
onpIns([A |T], ["cos" | T2]) -> onpIns([math:cos(A) | T], T2);
onpIns([A |T], ["tg" | T2]) -> onpIns([math:tan(A) | T], T2);
onpIns(L, [Sth| T2]) -> onpIns([toNum(Sth)|L], T2).

onp(Str) -> onpIns([], string:tokens(Str, " ")).

toNum(Str)->
	case string:to_float(Str) of
		{error,no_float} -> list_to_integer(Str);
		{F,_Rest} -> F
	end.
