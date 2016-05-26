-module(elambda_handle).
-export([handle/3]).


handle('POST', "verify_fibonacci", Req) ->
	handle('GET', "verify_fibonacci", Req);
handle('GET', "verify_fibonacci", Req) ->
	case Req:parse_qs() of
	[_|_]=QList ->
		next;
	_ ->
		QList = Req:parse_post()
	end,
	Lambda = proplists:get_value("lambda", QList),
	case catch gen_server:call(elambda_server,
				{verify_fibonacci,Lambda}) of
	{result,true,Counter} ->
		return(Req, integer_to_list(Counter));
	{result,false,_Counter} ->
		return(Req, "wrong answer");
	{result,timeout} ->
		return(Req, "time limit exceeded");
	_ ->
		return(Req, "unknown error")
	end;
handle('POST', "verify_factorial", Req) ->
	handle('GET', "verify_factorial", Req);
handle('GET', "verify_factorial", Req) ->
	case Req:parse_qs() of
	[_|_]=QList ->
		next;
	_ ->
		QList = Req:parse_post()
	end,
	Lambda = proplists:get_value("lambda", QList),
	case catch gen_server:call(elambda_server,
				{verify_factorial,Lambda}) of
	{result,true} ->
		return(Req, "accepted");
	{result,false} ->
		return(Req, "wrong answer");
	{result,timeout} ->
		return(Req, "time limit exceeded");
	_ ->
		return(Req, "unknown error")
	end;
handle('POST', "evaluate", Req) ->
	handle('GET', "evaluate", Req);
handle('GET', "evaluate", Req) ->
	case Req:parse_qs() of
	[_|_]=QList ->
		next;
	_ ->
		QList = Req:parse_post()
	end,
	Lambda = proplists:get_value("lambda", QList),
	case catch gen_server:call(elambda_server,
						{evaluate,Lambda}) of
	{result,Result,Counter} when is_integer(Result) ->
		RetData = "counter: \n" ++ integer_to_list(Counter)
				++ "   \n\nresult: \n" ++ integer_to_list(Result)
						++ "   \n\ncode: \n" ++ Lambda,
		return(Req, RetData);
	{result,Result,Counter} when is_atom(Result) ->
		RetData = "counter: \n" ++ integer_to_list(Counter)
				++ "   \n\nresult: \n" ++ atom_to_list(Result)
						++ "   \n\ncode: \n" ++ Lambda,
		return(Req, RetData);
	{result,timeout} ->
		return(Req, "time limit exceeded");
	_ ->
		return(Req, "unknown error")
	end;
handle(_Method, _Path, Req) ->
	return(Req, "invalid request!").


return(Req, Reply) ->
	Req:ok({"text/html; charset=utf-8",Reply}).

