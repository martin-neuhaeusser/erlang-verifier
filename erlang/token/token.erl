%% token.erl
%%
%% Example implementation of an algorithm that uses a cycling
%% token to ensure mutually exclusive access to a critical region

-module(token).
-export([start/0,start_station/1]) .

station(ReqCritSec, Next) ->
  receive
    {token, ReqCritSec} -> 
      io:format('pid ~w takes token for cs ~w\n', [self(), ReqCritSec]),
      %% enter critical section identified by ReqCritSec
      Next ! {token, ReqCritSec},
      station(ReqCritSec, Next);
    {token, CritSec} ->
      %% the token allows entering a critical section we are
      %% not interested in
      io:format('pid ~w passes token for cs ~w on \n', [self(), ReqCritSec]),
      Next ! {token, CritSec},
      station(ReqCritSec, Next)
  end.

start_station(ReqCritSec) ->
  receive
    {init, Next} -> station(ReqCritSec, Next)
  end.

create_tokens(Head, 0) ->
  Head ! {token, 0};
create_tokens(Head, Num) when Num > 0 ->
  Head ! {token, Num},
  create_tokens(Head, Num-1).
  
start() ->
  create_tokens(self(), 2),
  Station1 = spawn(token, start_station, [1]),
  Station2 = spawn(token, start_station, [2]),
  Station1 ! {init, Station2},
  Station2 ! {init, self()},
  station(0, Station1).
