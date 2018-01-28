(model-check(call 'token' : 'start' (),
	     scheduler(0, 1, 2) ->
	     [](receive(2, {'token', 2}) -> (((~ receive(0, {'token',2})) /\ ~(receive(1,{'token',2}))) U send(2,0,{'token',2}))) /\
	     [](receive(1, {'token', 1}) -> (((~ receive(0, {'token',1})) /\ ~(receive(2,{'token',1}))) U send(1,2,{'token',1}))) /\
	     [](receive(0, {'token', 0}) -> (((~ receive(1, {'token',0})) /\ ~(receive(2,{'token',0}))) U send(0,1,{'token',0})))))

(model-check(call 'token' : 'start' (),
	     scheduler(0, 1, 2) ->
	     [](<> receive(2, {'token', 2}) /\ <> receive(0, {'token',0}) /\ <> receive(1,{'token',1}))))
