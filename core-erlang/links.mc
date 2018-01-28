*** we always reach the point, where the initial process kills the first client
(model-check(call 'links' : 'start' ('kill'),
	     scheduler(0,1,2,3,4,5) -> (<> signal(0,1,{'EXIT', 0, 'kill'}))))

*** if we send only a 'normal'-exit signal, all processes stay alive (apart
*** from the always terminating initial process)
(model-check(call 'links' : 'start' ('normal'),
	     scheduler(0,1,2,3,4,5) ->
             <> ([] (running(1) /\ running(2) /\ running(3) /\ running(4) /\ running(5)))))
	    
*** if we send a kill signal (or any other non-'normal'-signal), all processes
*** but the locker process eventually terminate.
(model-check(call 'links' : 'start' ('kill'),
	     scheduler(0,1,2,3,4,5) -> (<> terminate(0)) 
	     /\ (<> terminate(1))
	     /\ (<> [] (running(5)))
	     /\ (<> [] ~(running(0) /\ running(1) /\ running(2) /\ running(3) /\ running(4)))))

*** validity check: a process having tau as its system label may not appear on the system level!
(model-check(call 'links' : 'start' ('normal'),
	     [] ~ tau-proc))
(model-check(call 'links' : 'start' ('kill'),
	     [] ~ tau-proc))
(model-check(call 'links' : 'start' ('some-error'),
	     [] ~ tau-proc))

