-module(audit_collector_tests).
-include_lib("eunit/include/eunit.hrl").

audit_collector_test_() ->
    {foreach,
     fun() -> audit_collector:start_link() end,
     fun(_) -> audit_collector:stop() end,
     [
      {"Process Send",fun audit_collector_process_send/0},
      {"Process Receive",fun audit_collector_process_receive/0},
      {"Process Started",fun audit_collector_process_started/0},
      {"Process Exited",fun audit_collector_process_exited/0},
      {"Process Started by Process Exited",fun audit_collector_process_exited_deep/0},
      {"Named Process Send",fun audit_collector_process_named_send/0},
      {"Named Process Receive",fun audit_collector_process_named_receive/0},
      {"Named Process Starts Process",fun audit_collector_process_named_started/0},
      {"Named Process Starts Named",fun audit_collector_process_named_started_named/0}
     ]}.

audit_collector_process_send() ->
    audit_collector:audit(process,[send]),
    Self = self(),
    Sender = spawn_link(fun() -> Self ! {i_sent_this,self()} end),
    receive {i_sent_this,Sender} -> ok end,
    timer:sleep(10),
    ?assertMatch(
       [{send,Sender,{i_sent_this,Sender}}],
       audit_collector:review(process,[send])).

audit_collector_process_receive() ->
    audit_collector:audit(process,['receive']),
    Receiver = spawn_link(fun() -> receive die -> ok end end),
    Receiver ! die,
    timer:sleep(10),
    ?assertMatch(
       [{'receive',Receiver,die}],
       audit_collector:review(process,['receive'])).
    
audit_collector_process_started() ->
    audit_collector:audit(process,[start]),
    Self = self(),
    Started = spawn(fun() -> Startee = spawn(lists,reverse,[[1,2,3]]), 
			     Self ! {started,Startee} end),
    {started,Startee} = receive X -> X end,
    timer:sleep(10),
    ?assertMatch(
       [{started,Started,Startee}],
       audit_collector:review(process,[start])).
    
audit_collector_process_exited() ->
    process_flag(trap_exit,true),
    audit_collector:audit(process,[exit]),
    Started = spawn_link(fun() -> hello end),
    receive 
	{'EXIT',Started,normal} ->
	    ok
    end,
    timer:sleep(10),
    ?assertMatch(
       [{exited,Started,normal}],
       audit_collector:review(process,[exit])).

audit_collector_process_exited_deep() ->
    process_flag(trap_exit,true),
    audit_collector:audit(process,[exit]),
    Self = self(),
    Main = spawn_link(fun() -> 
			      process_flag(trap_exit,true),
			      spawn_link(lists,reverse,[[1]]),
			      receive X -> Self ! X
			      end
		      end),
    {'EXIT',P2,normal} = (fun() -> receive X -> X end end)(),
    {'EXIT',Main,normal} = (fun() -> receive X -> X end end)(),
    timer:sleep(10),
    ?assertMatch(
       [{exited,P2,normal},
	{exited,Main,normal}
       ],
       audit_collector:review(process,[exit])).

audit_collector_process_named_send() ->
    audit_collector:audit(process,[named_send]),
    Self = self(),
    Sender = spawn_link(fun() -> register(iName,self()),
				 Self ! {i_sent_this,self()} 
			end),
    receive {i_sent_this,Sender} -> ok end,
    timer:sleep(10),
    ?assertMatch(
       [{send,iName,{i_sent_this,Sender}}],
       audit_collector:review(process,[named_send])).

audit_collector_process_named_receive() ->
    audit_collector:audit(process,[named_receive]),
    Self = self(),
    process_flag(trap_exit,true),
    Receiver = spawn_link(fun() -> register(iName,self()),
				   Self ! {started,self()},
				   receive die -> ok end 
			  end),
    receive {started,Receiver} -> ok end,
    Receiver ! die,
    receive {'EXIT',Receiver,normal} -> ok end,
    timer:sleep(10),
    ?assertMatch(
       [{'receive',iName,die}],
       audit_collector:review(process,[named_receive])).


audit_collector_process_named_started() ->
    audit_collector:audit(process,[named_start]),
    Self = self(),
    spawn_link(fun() ->
		       process_flag(trap_exit,true),
		       register(iName,self()),		  
		       Startee = spawn_link(lists,reverse,[[1,2,3]]),
		       receive {'EXIT',Startee,normal} -> ok end,
		       Self ! {started,Startee} 
	       end),
    {started,Startee} = receive X -> X end,
    timer:sleep(10),
    ?assertMatch(
       [{started,iName,Startee}],
       audit_collector:review(process,[named_start])).


audit_collector_process_named_started_named() ->
    audit_collector:audit(process,[named_start]),
    process_flag(trap_exit,true),
    P = spawn_link(fun() ->
			   process_flag(trap_exit,true),
			   register(iName,self()),
			   P2 = spawn_link(fun() -> register(iStarted,self()) end),
			   receive 
			       {'EXIT',P2,normal} -> ok
			   end
		   end),
    receive {'EXIT',P,normal} -> ok end,
    timer:sleep(10),
    ?assertMatch(
       [{started,iName,iStarted}],
       audit_collector:review(process,[named_start])).
   
