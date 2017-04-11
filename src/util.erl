%%%-------------------------------------------------------------------
%%% @author Paloma Piot.
%%% @copyright (C) 2017.
%%% @doc Practica de AS: Servicio de votaciones.
%%% Comunicación sockets.
%%% @end
%%% Created : 2017 by the authors.
%%%-------------------------------------------------------------------
 
-module(util).
 
%% Public API
-export([open/2, close/1, send/4]).

 
%%--------------------------------------------------------------------
%% @doc Function 'open'
%% @spec open(	Value1 :: Port
%%				Value2 :: Binary) -> {ok, socket}
%% @end
%% @doc Associates a UDP port number (Port) with the calling process.
%% @end
%%--------------------------------------------------------------------

open(Port, Binary) ->
	case Binary of
		true -> gen_udp:open(Port, [Binary, {active,true}]);
		false -> gen_udp:open(Port)
	end.
	
%%--------------------------------------------------------------------
%% @doc Function 'close'
%% @spec close(	Value1 :: socket) -> ok
%% @end
%% @doc Closes a UDP socket.
%% @end
%%--------------------------------------------------------------------

close(Socket) ->
	gen_udp:close(Socket).

%%--------------------------------------------------------------------
%% @doc Function 'send'
%% @spec send(	Value1 :: Socket
%%				Value2 :: IP
%%				Value3 :: Port
%%				Value4 :: Msg) -> ok | {error, Reason}
%% @end
%% @doc Sends a message to the specified ip and port.
%% @end
%%--------------------------------------------------------------------

send(Socket, IP, Port, Msg) ->
	gen_udp:send(Socket, IP, Port, Msg).
