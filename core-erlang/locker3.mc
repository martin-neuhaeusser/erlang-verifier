(model-check(call 'locker3' : 'start' (),
	     scheduler(0,1,2,3,4) ->
             []( send(0,2,'ok') -> (((~ receive(3,'ok')) /\ (~ receive(4,'ok'))) U (receive(0,{'rel',2})))) /\
	     []( send(0,3,'ok') -> (((~ receive(2,'ok')) /\ (~ receive(4,'ok'))) U (receive(0,{'rel',3})))) /\
	     []( send(0,4,'ok') -> (((~ receive(2,'ok')) /\ (~ receive(3,'ok'))) U (receive(0,{'rel',4}))))))
	    

*** Fairness property with a fair scheduler
(model-check(call 'locker3' : 'start' (),
	     scheduler(0,1,2,3,4) ->
	     []((<> send(0,2,'ok')) /\ (<> send(0,3,'ok')) /\ (<> send(0,4,'ok')))))

(model-check(call 'locker3' : 'start' (),
	     scheduler(0,1,2,3,4) -> <> newproc(1)))

