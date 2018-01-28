-module(locker).
-export([start/0,locker/1,client/2,heads/1,tails/1]).

%
% Frame
%

  start() ->
    Locker = self(),
    start_client([a],Locker),
    start_client([b],Locker),
    start_client([a,b],Locker),
    locker(state([a,b])) .

%
% Locker
%
    state([]) ->
      [];
    state([R|Rs]) ->
      [{R,none,[]}|state(Rs)].

    locker(State) ->
      receive
        {request,Client,Resources} ->
          case available(State,Resources) of
            true ->
              Client!ok,
              locker(grant(State,Client,Resources));
            false ->
              locker(wait(State,Client,Resources))
          end;
        {release,Client} ->
          locker(release(State,Client))
      end.
%
% test whether resources are available
%
    available(State,[]) ->
      true;
    available(State,[R|Rs]) ->
      avail(State,R) and available(State,Rs).

    avail([],R) ->
      false;
    avail([{R,none,[]}|Ss],R) ->
      true;
    avail([{R,_,_}|Ss],R) ->
      false;
    avail([_|Ss],R) ->
      avail(Ss,R).
%
% grant access to client
%
    grant(State,Client,[]) ->
      State;
    grant(State,Client,[R|Rs]) ->
      grant(update(State,Client,R),Client,Rs).

    update([{R,none,[]}|Ss],Client,R) ->
      [{R,Client,[]}|Ss];
    update([S|Ss],Client,R) ->
      [S|update(Ss,Client,R)].
%
% enter client into waiting lists
%
    wait(State,Client,[]) ->
      State;
    wait(State,Client,[R|Rs]) ->
      wait(insert(State,Client,R),Client,Rs).

    insert([{R,A,Cs}|Ss],Client,R) ->
      [{R,A,lists:append(Cs,[Client])}|Ss];
    insert([S|Ss],Client,R) ->
      [S|insert(Ss,Client,R)].
%
% release resources held by client
%
    release(State,Client) ->
      New = free(State,Client),
      next(New).
%
% remove client from Access field
%
    free([],Client) ->
      [];
    free([{R,Client,Cs}|Ss],Client) ->
      [{R,none,Cs}|free(Ss,Client)];
    free([S|Ss],Client) ->
      [S|free(Ss,Client)].
%
% determine next clients
%
    next(State) ->
      Heads = heads(State),
      Tails = tails(State),
      Candidates = Heads -- Tails,
      grants(State,Candidates).

    heads([]) ->
      [];
    heads([{R,none,[C|Cs]}|Ss]) ->
      Hs = heads(Ss),
      case lists:member(C,Hs) of
        true ->
          Hs;
        false ->
          [C|Hs]
      end;
    heads([_|Ss]) ->
      heads(Ss).
   
    tails([]) ->
      [];
    tails([{R,A,[]}|Ss]) ->
      tails(Ss);
    tails([{R,none,[C|Cs]}|Ss]) ->
      Cs ++ tails(Ss);
    tails([{R,_,[C|Cs]}|Ss]) ->
      [C|Cs] ++ tails(Ss).

    grants(State,[]) ->
      State;
    grants(State,[C|Cs]) ->
      grants(grant(State,C),Cs).

    grant([],Client) ->
      Client!ok,
      [];
    grant([{R,none,[Client|Cs]}|Ss],Client) ->
      [{R,Client,Cs}|grant(Ss,Client)];
    grant([S|Ss],Client) ->
      [S|grant(Ss,Client)].
   
%
% Client
%

    start_client(Resources,Locker) ->
      spawn(locker,client,[Resources,Locker]).

    client(Resources,Locker) ->
      Client = self(),
      Locker!{request,Client,Resources},
      receive
        ok ->
          % critical section
          Locker!{release,Client},
          client(Resources,Locker)
      end.
