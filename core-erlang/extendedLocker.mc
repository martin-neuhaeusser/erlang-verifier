(model-check(call 'locker' : 'start' (),
	     scheduler(0,1,2,3) ->
             []( send(0,1,'ok') -> ((~ receive(3,'ok')) U (receive(0,{'release',1})))) /\
	     []( send(0,2,'ok') -> ((~ receive(3,'ok')) U (receive(0,{'release',2})))) /\
	     []( send(0,3,'ok') -> (((~ receive(1,'ok')) /\ (~ receive(2,'ok'))) U (receive(0,{'release',3}))))))
	    
*** Fairness property with a fair scheduler
(model-check(call 'locker' : 'start' (),
	     scheduler(0,1,2,3) ->
	     []((<> send(0,1,'ok')) /\ (<> send(0,2,'ok')) /\ (<> send(0,3,'ok')))))

(model-check(call 'locker' : 'start' (),
	     scheduler(0,1,2,3) -> <> newproc(1)))

*** validity check: a process having tau as its system label may not appear on the system level!
(model-check(call 'locker' : 'start' (),
	     [] ~ tau-proc))

