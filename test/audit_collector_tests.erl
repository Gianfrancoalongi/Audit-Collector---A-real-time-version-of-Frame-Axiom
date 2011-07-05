-module(audit_collector_tests).
-include_lib("eunit/include/eunit.hrl").

audit_collector_test_() ->
    {foreach,
     fun() -> audit_collector:start_link() end,
     fun(P) -> audit_collector:stop() end,
     [fun audit_collector_process/0
     ]}.

audit_collector_process() ->
    audit_collector:audit(process,[send,'receive']),
    Self = self(),
    Sender = spawn_link(fun() -> Self ! {i_sent_this,self()} end),
    Receiver = spawn_link(fun() -> receive die -> ok end end),
    receive {i_sent_this,Sender} -> ok end,
    Receiver ! die,
    ?assertMatch(
       [{send,Sender,{i_sent_this,Sent}},
	{'receive',Receiver,die}],
       audit_collector:review(process,[send,'receive'])).
