-module(elambda_cfg).
-compile(export_all).


%% inet addr
get_addr() ->
	{mochiaddr,[
		 {ip,"0.0.0.0"}
		,{port,10086}]
	}.

%% timeout (ms)
get_timeout() ->
	1000.

%% fib test cases
get_fibargs() ->
	[
		0,
		1,
		2,
		3,
		4,
		5,
		10,
		20,
		50,
		100,
		200,
		500,
		1000,
		2000,
		5000
	].




