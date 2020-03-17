-module(mfun).
-author("Agnieszka Dutka").

%% API
-export([map/2, filter/2, sumDigits/1, div3NoRem/1]).

map(F, List) -> [F(X) || X <-List].
filter(F, List) -> [X || X<-List, F(X)].

sumDigits(Nr)->lists:foldl(fun(X, Y)-> X+Y end, 0, toList(abs(Nr))).
toList(Nr) when Nr <10 -> [Nr];
toList(Nr) -> [Nr rem 10 | toList(Nr div 10)].

div3NoRem(List) -> lists:filter(fun(X) ->sumDigits(X) rem 3 ==0 end,List).