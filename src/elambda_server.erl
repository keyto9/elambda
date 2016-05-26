-module(elambda_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


%%%============================================================================
%%% API functions
%%%============================================================================
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).


%%%============================================================================
%%% Behaviour functions
%%%============================================================================
init([]) ->
	process_flag(trap_exit, true),
	{ok,0}.


handle_call(i, _From, State) ->
	{reply,State,State,hibernate};
handle_call({evaluate,Lambda}, _From, State) ->
	case catch elambda:evaluate(Lambda) of
	{'EXIT',_ErrTypeReason} ->
		Result = {result,unknown};
	Result ->
		next
	end,
	{reply,Result,State+1,hibernate};
handle_call({verify_factorial,Lambda}, _From, State) ->
	case catch elambda:verify_factorial(Lambda) of
	{'EXIT',_ErrTypeReason} ->
		Result = {result,unknown};
	Result ->
		next
	end,
	{reply,Result,State+1,hibernate};
handle_call({verify_fibonacci,Lambda}, _From, State) ->
	case catch elambda:verify_fibonacci(Lambda) of
	{'EXIT',_ErrTypeReason} ->
		Result = {result,unknown};
	Result ->
		next
	end,
	{reply,Result,State+1,hibernate};
handle_call(_Request, _From, State) ->
	{noreply,State,hibernate}.


handle_cast(_Request, State) ->
	{noreply,State}.


handle_info(_Info, State) ->
	{noreply,State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok,State}.


%%%============================================================================
%%% Internal functions
%%%============================================================================

