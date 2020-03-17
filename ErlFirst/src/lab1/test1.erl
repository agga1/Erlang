%%%-------------------------------------------------------------------
%%% @author Agnieszka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2020 15:54
%%%-------------------------------------------------------------------
%% slownik - lista kortek klucz wartosc
-module(test1).
-author("Agnieszka").

%% API
-export([fac/1, greeting/0, power/2, pow/2]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).
%%
greeting() -> io:fwrite("hello").

power(_, 0) -> 1;
power(X, N) when is_integer(N) and is_number(X)-> X*power(X, N-1);
power(_, _) -> io:fwrite("wrong arguments").

pow(X, N) -> math:pow(X, N).

%%
%%fibo(0) -> 0;
%%fibo(n) -> n.
%%fibo(n) when n>0 -> fibo(n-1) + fibo(n-2).
