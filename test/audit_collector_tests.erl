-module(audit_collector_tests).
-include_lib("eunit/include/eunit.hrl").

audit_collector_test_() ->
    {foreach,
     fun() -> audit_collector:start_link() end,
     fun(_) -> audit_collector:stop() end,
     [fun audit_collector_process_send/0,
      fun audit_collector_process_receive/0
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
    
