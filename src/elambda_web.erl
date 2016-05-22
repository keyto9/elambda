-module(elambda_web).
-export([start/1,port_loop/2]).


start(Options) ->
    [{docroot,DocRoot}|OptionsT] = Options,
    PortLoop = fun(Req) -> ?MODULE:port_loop(Req, DocRoot) end,
    lists:foldl(
        fun({Name,Option}, _Acc) ->
                mochiweb_http:start([{name,Name},{loop,PortLoop}|Option])
        end, undefined, OptionsT).


port_loop(Req, _DocRoot) ->
    Method = Req:get(method),
    Path = string:strip(Req:get(path), both, $/),
    case catch elambda_handle:handle(Method, Path, Req) of
    {'EXIT',_ErrTypeReason} ->
        Req:respond({500,[{"Content-Type","text/plain"}]
                            ,"request failed, sorry\n"});
    _ ->
        ignore
    end.

