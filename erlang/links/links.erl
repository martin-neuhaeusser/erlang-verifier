-module(links).
-export([start/1,observer/0,client/0]).

observer() ->
  process_flag(trap_exit, true),
  receive
    {observe, Sender, ClientPid} ->
      link(ClientPid),
      Sender ! {ack, self()},
      observer();
    {'EXIT', Pid, Reason} ->
      io:format("observer receives EXIT-signal from PID ~w with reason:~w~n",[Pid, Reason]),
      observer()
  end.

client() ->
  receive
    {setup_link, Sender, Peer} -> 
      link(Peer),
      Sender ! {ack, self()},
      client();
    {'EXIT', Pid, Reason} ->
      io:format("client ~w receives EXIT-signal from ~w:~w~n", [self(), Pid, Reason]),
      client()
  end.
    
create_clients(0) -> [];
create_clients(Count) -> 
  [spawn(links, client, []) | create_clients(Count-1)].

link_clients([]) -> true;
link_clients([C | Cs]) -> 
  link_client(C, Cs), 
  link_clients(Cs).

link_client(C1, []) -> true;
link_client(C1, [C2|Cs]) -> 
  C1 ! {setup_link, self(), C2},
  receive
    {ack, C1} -> link_client(C1, Cs)
  end.

observe_clients(Obs, []) -> true;
observe_clients(Obs, [C|Cs]) -> 
  Obs ! {observe, self(), C},
  receive 
    {ack, Obs} -> observe_clients(Obs, Cs)
  end.

start(Reason) ->
  ClientList = create_clients(4),
  io:format("client list is: ~w~n", [ClientList]),
  link_clients(ClientList),
  Obs = spawn(links, observer, []),
  observe_clients(Obs, ClientList),
  io:format("main process killing client ~w with exit-signal: ~w~n", [hd(ClientList), Reason]),
  exit(hd(ClientList), Reason).
