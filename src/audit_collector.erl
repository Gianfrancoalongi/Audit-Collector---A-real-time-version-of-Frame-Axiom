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

-spec(audit(process,[send|'receive'|start]) -> ok).	     
audit(process,Options) ->
    gen_server:call(?MODULE,{add_tracing_on,{process,Options}}).

-spec(review(process,[send|'receive'|start]) -> [tuple()]).
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
    OPFlags = options_to_trace_flag(Options),
    Flags = [timestamp,{tracer,self()}|OPFlags],
    erlang:trace(new,true,Flags), 
    {reply,ok,State};
handle_call({review,{process,Options}},_,State) ->
    Reply = make_log(Options,lists:reverse(State#state.log)),    
    {reply,Reply,State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(X, State) ->
    {noreply, State#state{log = [X|State#state.log]}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
options_to_trace_flag([start|R]) ->
    [procs,set_on_spawn|options_to_trace_flag(R)];
options_to_trace_flag([exit|R]) ->
    [procs,set_on_spawn|options_to_trace_flag(R)];
options_to_trace_flag([named_send|R]) ->
    [procs,set_on_spawn,send|options_to_trace_flag(R)];
options_to_trace_flag([named_receive|R]) ->
    [procs,set_on_spawn,'receive'|options_to_trace_flag(R)];
options_to_trace_flag([X|R]) -> 
    [X|options_to_trace_flag(R)];
options_to_trace_flag([]) -> [].

make_log([],_) -> [];
make_log([send|R],History) ->
    lists:foldl(
      fun({trace_ts,P,send,Msg,To,_},Acc) when is_pid(P)->
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
	      case Msg of
		  {io_reply,P2,_} when is_pid(P2)-> 
		      Acc;
		  _ ->
		      Acc++[{'receive',P,Msg}]
		 end;
	 (_,Acc) -> Acc		       
      end,[],History)++make_log(R,History);
make_log([start|R],History) ->
    lists:foldl(
      fun({trace_ts,P,spawn,P2,{_M,_F,_A},_},Acc) ->
	      Acc++[{started,P,P2}];
	 (_,Acc) -> Acc
      end,[],History)++make_log(R,History);
make_log([exit|R],History) ->
    lists:foldl(
      fun({trace_ts,P,exit,C,_},Acc) ->
	      Acc++[{exited,P,C}];
	 (_,Acc) ->
	      Acc
      end,[],History)++make_log(R,History);
make_log([named_send|R],History) ->
    Registered = lists:foldl(
		   fun({trace_ts,P,register,Name,_},Acc) ->
			   Acc++[{P,Name}];
		      (_,Acc) -> Acc
		   end,[],History),
    Sent = make_log([send],History),
    lists:foldl(
      fun({send,P,Msg},Acc) ->
	      case proplists:get_value(P,Registered) of
		  undefined ->
		      Acc;
		  Name -> Acc++[{send,Name,Msg}]
	      end
      end,[],Sent)++make_log(R,History);
make_log([named_receive|R],History) ->
    Registered = lists:foldl(
		   fun({trace_ts,P,register,Name,_},Acc) ->
			   Acc++[{P,Name}];
		      (_,Acc) -> Acc
		   end,[],History),
    Received = make_log(['receive'],History),
    lists:foldl(
      fun({'receive',P,Msg},Acc) ->
	      case proplists:get_value(P,Registered) of
		  undefined ->
		      Acc;
		  Name -> Acc++[{'receive',Name,Msg}]
	      end
      end,[],Received)++make_log(R,History).
