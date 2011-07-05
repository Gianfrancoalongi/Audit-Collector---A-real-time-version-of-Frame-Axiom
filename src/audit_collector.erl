-module(audit_collector).
-behaviour(gen_server).

%% API
-export([start_link/0,
	 stop/0]).
-export([audit/2]).
-export([review/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {log :: list()
	       }).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(stop() -> ok).	     
stop() ->
    gen_server:call(?MODULE,stop).

-spec(audit(process,[send|'receive']) -> ok).	     
audit(process,Options) ->
    gen_server:call(?MODULE,{add_tracing_on,{process,Options}}).

-spec(review(process,[send|'receive']) -> [tuple()]).
review(process,Options) ->	     
    gen_server:call(?MODULE,{review,{process,Options}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{log = []
	       }}.

handle_call(stop,_, State) ->
    {stop,normal,ok,State};
handle_call({add_tracing_on,{process,Options}},_,State) ->
    OPFlags = option_to_flag(Options),
    Flags = [timestamp,{tracer,self()}|OPFlags],
    erlang:trace(new,true,Flags), 
    {reply,ok,State};
handle_call({review,{process,Options}},_,State) ->
    OPFlags = option_to_flag(Options),
    io:format(user,"Audit Collector :: review, {process,OPFlags} ---> ~p~n",[OPFlags]),
    io:format(user,"Audit Collector :: State log ---> ~p~n",[State#state.log]),
    Reply = make_log(OPFlags,lists:reverse(State#state.log)),
    io:format(user,"Reply == ~p~n",[Reply]),
    {reply,Reply,State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(X, State) ->
    io:format(user,"Audit Collector :: handle_info/2 -> ~p~n",[X]),
    {noreply, State#state{log = [X|State#state.log]}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
option_to_flag(X) ->
    X.

make_log([],_) -> [];
make_log([send|R],History) ->
    lists:foldl(
      fun({trace_ts,P,send,Msg,To,_},Acc) when is_pid(P)->
io:format(user,"Foldl SEND ~p~n",[{trace_ts,P,send,Msg,To}]),
	      case Msg of
		  {io_request,P,To,_} -> 
		      Acc;
		  _ ->
		      Acc++[{send,P,Msg}]
	      end;
	 (_,Acc) -> Acc
      end,[],History)++make_log(R,History);
make_log(['receive'|R],History) ->
    lists:foldl(
      fun({trace_ts,P,'receive',Msg,_},Acc) ->
io:format(user,"Foldl RECEIVE ~p~n",[{trace_ts,P,'receive',Msg}]),
	      case Msg of
		  {io_reply,P2,_} when is_pid(P2)-> 
		      Acc;
		  _ ->
		      Acc++[{'receive',P,Msg}]
		 end;
	 (_,Acc) -> Acc		       
	 end,[],History)++make_log(R,History).

