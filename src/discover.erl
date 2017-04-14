%%%-------------------------------------------------------------------
%%% @author Daniel Gabín.
%%% @copyright (C) 2017.
%%% @doc Practica de AS: Servicio de votaciones.
%%% Nodo de descubrimiento - Primera funcionalidad.
%%% @end
%%% Created : 2017 by the authors.
%%%-------------------------------------------------------------------
 
-module(discover).

%% Public API
-export([start/1]).


%%--------------------------------------------------------------------
%% @doc Function 'start'
%% @spec start(	Value1 :: Port)
%% @end
%% @doc Start discover node listening service in the giving port.
%% @end
%%--------------------------------------------------------------------

start(Port) ->
    spawn(fun() -> init(Port) end).


init(Port) ->
	{ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
	io:format("Server opened socket:~p~n",[Socket]),
	loop(Socket).


loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    L = [1,2,3],
    receive
        {udp, Socket, IP, Port, Msg} ->
            io:format("Server received:~p~n",[Msg]),
            case [Msg] of
            	[<<"request">>] -> util:send(Socket, IP, Port, L);
            	_ -> util:send(Socket, IP, Port, "error")
            end,
            loop(Socket)
    end.



