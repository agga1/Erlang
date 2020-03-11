%%%-------------------------------------------------------------------
%%% @author Agnieszka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 12:15
%%%-------------------------------------------------------------------
-module(onp).
-author("Agnieszka").

%% API
-export([onpIns/2, onp/1, toNum/1]).

onpIns([H|_], []) -> H;
onpIns([A, B |T], ["+" | T2]) -> onpIns([B+A | T], T2);
onpIns([A, B |T], ["-" | T2]) -> onpIns([B-A | T], T2);
onpIns([A, B |T], ["*" | T2]) -> onpIns([B*A | T], T2);
onpIns([A, B |T], ["/" | T2]) -> onpIns([B/A | T], T2);
onpIns(L, [Sth| T2]) -> onpIns([toNum(Sth)|L], T2).
onp(Str) -> onpIns([], string:tokens(Str, " ")).

toNum(Str)->
  case string:to_float(Str) of
    {error,no_float} -> list_to_integer(Str);
    {F,_Rest} -> F
  end.
%% zrobic bez funkjci wyzszego rzedu z floatem, sqrt, pow, sin, cos
