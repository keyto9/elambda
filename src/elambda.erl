-module(elambda).
-export([start/0]).
-export([verify_fibonacci/1,verify_factorial/1,evaluate/1]).
-compile(export_all).


-define(MOD_ATOM, 'ad148a3ca8bd0ef3b48c52454c493ec5').
-define(MOD_STR, "ad148a3ca8bd0ef3b48c52454c493ec5").
-define(FUN_ATOM, '77004ea213d5fc71acf74a8c9c6795fb').
-define(FUN_STR, "77004ea213d5fc71acf74a8c9c6795fb").
-define(isdigit(C), (($0 =< C) andalso (C =< $9))).
-define(isalpha(C), ((($A =< C) andalso (C =< $Z))
			orelse (($a =< C) andalso (C =< $z)))).
-define(isunder(C), (C =:= $_)).
-define(isleftp(C), (C =:= $()).
-define(isrightp(C), (C =:= $))).
-define(isbackslash(C), (C =:= $\\)).
-define(isdot(C), (C =:= $.)).
-define(isblank(C), ((C =:= 32) orelse (C =:= 9)
			orelse (C =:= 10) orelse (C =:= 13))).
-define(KEY_WORDS, ["add","sub","mul","div"
			,"cond","less","true","false"]).


start() ->
	application:start(?MODULE).


verify_fibonacci(Lambda) when is_list(Lambda) ->
	erlang:process_flag(trap_exit, true),
	Timeout = elambda_cfg:get_timeout(),
	flush_msg(),
	Ref = erlang:make_ref(),
	Father = erlang:self(),
	Pid = erlang:spawn_link(fun() ->
		gen(Lambda),
		erlang:put(counter, 0),
		Fibonacci = ?MOD_ATOM:?FUN_ATOM(),
		Result = lists:all(fun(N) ->
			fibonacci(N) =:= Fibonacci(N)
		end, elambda_cfg:get_fibargs()),
		Counter = erlang:get(counter),
		Father ! {result,Ref,Result,Counter}
	end),
	receive
	{result,Ref,Result,Counter} ->
		{result,Result,Counter};
	{'EXIT',Pid,_ErrTypeReason} ->
		{result,unknown}
	after Timeout ->
		case erlang:is_process_alive(Pid) of
		true ->
			exit(Pid, kill),
			{result,timeout};
		_ ->
			{result,unknown}
		end
	end.


verify_factorial(Lambda) when is_list(Lambda) ->
	erlang:process_flag(trap_exit, true),
	Timeout = elambda_cfg:get_timeout(),
	flush_msg(),
	Ref = erlang:make_ref(),
	Father = erlang:self(),
	Pid = erlang:spawn_link(fun() ->
		gen(Lambda),
		Factorial = ?MOD_ATOM:?FUN_ATOM(),
		Result = lists:all(fun(_Idx) ->
			<<N:8>> = crypto:rand_bytes(1),
			factorial(N) =:= Factorial(N)
		end, lists:seq(1, 20)),
		Father ! {result,Ref,Result}
	end),
	receive
	{result,Ref,Result} ->
		{result,Result};
	{'EXIT',Pid,_ErrTypeReason} ->
		{result,unknown}
	after Timeout ->
		case erlang:is_process_alive(Pid) of
		true ->
			exit(Pid, kill),
			{result,timeout};
		_ ->
			{result,unknown}
		end
	end.


evaluate(Lambda) when is_list(Lambda) ->
	erlang:process_flag(trap_exit, true),
	Timeout = elambda_cfg:get_timeout(),
	flush_msg(),
	Ref = erlang:make_ref(),
	Father = erlang:self(),
	Pid = erlang:spawn_link(fun() ->
		gen(Lambda),
		erlang:put(counter, 0),
		Result = ?MOD_ATOM:?FUN_ATOM(),
		Counter = erlang:get(counter),
		Father ! {result,Ref,Result,Counter}
	end),
	receive
	{result,Ref,Result,Counter} ->
		{result,Result,Counter};
	{'EXIT',Pid,_ErrTypeReason} ->
		{result,unknown}
	after Timeout ->
		case erlang:is_process_alive(Pid) of
		true ->
			exit(Pid, kill),
			{result,timeout};
		_ ->
			{result,unknown}
		end
	end.


flush_msg() ->
	receive _ ->
		flush_msg()
	after 0 ->
		ok
	end.


fibonacci(N) when is_integer(N), N >= 0 ->
	fibonacci(N, 0, 1).
fibonacci(0, A, _B) ->
	A;
fibonacci(N, A, B) ->
	fibonacci(N-1, B, A+B).


factorial(N) when is_integer(N), N >= 0 ->
	factorial(N, 1).
factorial(0, A) ->
	A;
factorial(N, A) ->
	factorial(N-1, A*N).


gen(Lambda) when is_list(Lambda) ->
	CodeStr = translate(Lambda),
	{?MOD_ATOM,CodeBin}
		= dynamic_compile:from_string(CodeStr),
	code:load_binary(?MOD_ATOM, ?MOD_STR".erl", CodeBin).


modhead() ->
"
-module( '" ?MOD_STR "' ).
-export(['"?FUN_STR"'/0]).

'"?FUN_STR"'() ->

".


modtail() ->
"
.


'add'(X) ->
	fun(Y) -> X + Y end.
'sub'(X) ->
	fun(Y) -> X - Y end.
'mul'(X) ->
	fun(Y) -> X * Y end.
'div'(X) ->
	fun(Y) -> X div Y end.
'cond'(Cond) ->
	fun(Truly) ->
		fun(Falsy) ->
			case Cond of
			true ->
				Truly(undefined);
			false ->
				Falsy(undefined)
			end
		end
	end.
'less'(X) ->
	fun(Y) ->
		X < Y
	end.


update_counter() ->
	case erlang:get(counter) of
	Counter when is_integer(Counter) ->
		case (Counter rem 5 =:= 0) of
		true ->
			{memory,Memory} = erlang:process_info(self(), memory),
			case (Memory > 999999) of
			true ->
				erlang:exit(memory_limit);
			_ ->
				ignore
			end;
		_ ->
			ignore
		end,
		erlang:put(counter, Counter+1);
	_ ->
		erlang:put(counter, 1)
	end.


".


translate(Lambda) ->
	Tokens = tokens(Lambda),
	case accept_expression(Tokens, "", "") of
	{ok,Body,[]} ->
		CodeStr = modhead() ++ Body ++ modtail(),
		CodeStr2 = [C || C <- CodeStr, C =/= 13],
		CodeStr2;
	_ ->
		erlang:throw({accept_error})
	end.


accept_expression(Tokens, HeadPart, TailPart) ->
	case Tokens of
	[{variable,_}|_] ->
		accept_variable(Tokens, HeadPart, TailPart);
	[{number,_}|_] ->
		accept_number(Tokens, HeadPart, TailPart);
	[{backslash,_},{variable,_},{dot,_}|_] ->
		accept_function(Tokens, HeadPart, TailPart);
	[{leftp,_}|_] ->
		accept_application(Tokens, HeadPart, TailPart);
	_ ->
		erlang:throw({accept_error})
	end.


accept_variable(Tokens, HeadPart, TailPart) ->
	case Tokens of
	[{variable,Var}|Rest] ->
		HeadAdd = varfmt(Var), TailAdd = "",
		HeadPart2 = HeadPart ++ HeadAdd,
		TailPart2 = TailAdd ++ TailPart,
		{ok,HeadPart2++TailPart2,Rest};
	_ ->
		erlang:throw({accept_error})
	end.


varfmt(Var) ->
	case lists:member(Var, ?KEY_WORDS) of
	true ->
		"'"++Var++"'";
	_ ->
		"X" ++ Var
	end.


accept_number(Tokens, HeadPart, TailPart) ->
	case Tokens of
	[{number,Number}|Rest] ->
		HeadAdd = [Number], TailAdd = "",
		HeadPart2 = HeadPart ++ HeadAdd,
		TailPart2 = TailAdd ++ TailPart,
		{ok,HeadPart2++TailPart2,Rest};
	_ ->
		erlang:throw({accept_error})
	end.


accept_function(Tokens, HeadPart, TailPart) ->
	case Tokens of
	[{backslash,_},{variable,Var},{dot,_}|T] ->
		HeadAdd = "( fun( " ++ argfmt(Var) ++ " ) -> ",
		TailAdd = " end )",
		HeadPart2 = HeadPart ++ HeadAdd,
		TailPart2 = TailAdd ++ TailPart,
		accept_expression(T, HeadPart2, TailPart2);
	_ ->
		erlang:throw({accept_error})
	end.


argfmt(Var) ->
	case lists:member(Var, ?KEY_WORDS) of
	true ->
		erlang:throw({accept_error});
	_ ->
		"X" ++ Var
	end.


accept_application(Tokens, HeadPart, TailPart) ->
	case Tokens of
	[{leftp,_}|T] ->
		HeadAdd = "(begin update_counter(), ", TailAdd = " end)",
		HeadPart2 = HeadPart ++ HeadAdd,
		TailPart2 = TailAdd ++ TailPart,
		{ok,FunExp,T2} = accept_expression(T, "", ""),
		{ok,ArgExp,T3} = accept_expression(T2, "", ""),
		[{rightp,_}|Rest] = T3,
		Body = FunExp ++ "( " ++ ArgExp ++ " )",
		{ok,HeadPart2++Body++TailPart2,Rest};
	_ ->
		erlang:throw({accept_error})
	end.


tokens(Str) ->
	tokens(Str++" ", undefined, []).
tokens([], undefined, Acc) ->
	lists:reverse(Acc);
tokens([H|T], undefined, Acc) ->
	if ?isalpha(H); ?isunder(H) ->
		tokens(T, {variable,[H]}, Acc);
	?isdigit(H) ->
		tokens(T, undefined, [{number,H}|Acc]);
	?isleftp(H) ->
		tokens(T, undefined, [{leftp,H}|Acc]);
	?isrightp(H) ->
		tokens(T, undefined, [{rightp,H}|Acc]);
	?isbackslash(H) ->
		tokens(T, undefined, [{backslash,H}|Acc]);
	?isdot(H) ->
		tokens(T, undefined, [{dot,H}|Acc]);
	?isblank(H) ->
		tokens(T, undefined, Acc);
	true ->
		erlang:throw({token_error})
	end;
tokens([H|T]=L, {variable,V}, Acc) ->
	if ?isalpha(H); ?isunder(H); ?isdigit(H) ->
		tokens(T, {variable,V++[H]}, Acc);
	?isleftp(H); ?isrightp(H); ?isbackslash(H)
	; ?isdot(H); ?isblank(H) ->
		tokens(L, undefined, [{variable,V}|Acc]);
	true ->
		erlang:throw({token_error})
	end.

