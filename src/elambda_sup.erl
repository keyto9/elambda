-module(elambda_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).


init([]) ->
	Processes = [server_specs(),web_specs(elambda_web)],
	{ok,{{one_for_one,10,10},Processes}}.


web_specs(Mod) ->
	WebConfig = [{docroot,"undefined"}|[elambda_cfg:get_addr()]],
	{Mod,{Mod,start,[WebConfig]},permanent,5000,worker,dynamic}.


server_specs() ->
	{elambda_server,{elambda_server,start_link,[]}
		,permanent,60000,worker,[elambda_server]}.
