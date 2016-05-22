-module(elambda_cfg).
-compile(export_all).


get_addr() ->
	{mochiaddr,[
		 {ip,"0.0.0.0"}
		,{port,10086}]
	}.

