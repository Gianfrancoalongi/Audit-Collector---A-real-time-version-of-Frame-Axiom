-module(audit_collector_tests).
-include_lib("eunit/include/eunit.hrl").

audit_collector_test_() ->
    {foreach,
     fun() -> audit_collector:start_link() end,
     fun(_) -> audit_collector:stop() end,
     [
      fun audit_collector_process_send/0,
      fun audit_collector_process_receive/0,
      fun audit_collector_process_started/0,
      fun audit_collector_process_exited/0,
      fun audit_collector_process_exited_deep/0,
      fun audit_collector_process_named_send/0,
      fun audit_collector_process_named_receive/0,
      fun audit_collector_process_named_started/0
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
			      Alt = spawn_link(lists,reverse,[[1]]),
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
    Started = spawn(fun() ->
			    register(iName,self()),
			    Startee = spawn(lists,reverse,[[1,2,3]]),
			    Self ! {started,Startee} end),
    {started,Startee} = receive X -> X end,
    timer:sleep(10),
    ?assertMatch(
       [{started,iName,Startee}],
       audit_collector:review(process,[named_start])).
    
