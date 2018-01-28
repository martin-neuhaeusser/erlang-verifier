-module(locker2).
-export([start/0,client/1,locker/0]).

locker() ->
  receive
    {req, Client} -> 
      Client ! ok,
      receive
        {rel, Client} -> locker()
      end
  end.

client(Locker) ->
  Client = self(),
  Locker ! {req, Client},
  receive
    ok -> 
      %% critical section 
      Locker ! {rel, Client}
  end,
  client(Locker) .

start() -> 
  LockerPid = self(),
  spawn(locker2, client, [LockerPid]),
  spawn(locker2, client, [LockerPid]),
  locker() .  
