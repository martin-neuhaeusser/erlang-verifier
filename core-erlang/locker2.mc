(model-check(call 'locker2' : 'start' (),
	     scheduler(0, 1, 2) ->
             []( send(0,1,'ok') -> ((~ receive(2,'ok')) U receive(0,{'rel',1}))) /\
	     []( send(0,2,'ok') -> ((~ receive(1,'ok')) U receive(0,{'rel',2})))))

*** Fairness property with a fair scheduler
(model-check(call 'locker2' : 'start' (),
	     scheduler(0, 1, 2) ->
	     []((<> send(0,1,'ok')) /\ (<> send(0,2,'ok')))))

(model-check(call 'locker2' : 'start' (),
	     scheduler(0, 1, 2) -> <> newproc(1)))

