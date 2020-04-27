-module(pow).
-author("Agnieszka Dutka").
-behaviour(gen_server).
%% API
-export([start_link/0, step/0, read/0, close/0, crash/0]).
-export([init/1, handle_call/3, terminate/2, handle_cast/2]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE},?MODULE,2,[]).
init(N)        -> {ok,N}.

%% INTERFEJS KLIENT -> SERWER %%
step()      -> gen_server:cast(?MODULE,step).
read()      -> gen_server:call(?MODULE,read).
close()     -> gen_server:call(?MODULE,terminate).
crash()     -> gen_server:cast(?MODULE,crash).

%% OBSŁUGA WIADOMOŚCI %%
handle_cast(step, N) -> {noreply, N*N};
handle_cast(crash, N) -> no:exist(), {noreply, N}.

handle_call(read,_From, N)      -> {reply, N, N};
handle_call(terminate,_From,N) -> {stop, normal, ok, N}.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.
