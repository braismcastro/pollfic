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
-export([open/2, close/1, send/4, term_to_str/1, str_to_term/1,send_file/3, save_file/2, receive_file/2 ]).
-define(KEYFOLDER, "../keys/").
 
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

term_to_str(Term) -> 
	io_lib:fwrite("~p.", [Term]).

str_to_term(eof) -> eof;
str_to_term(Str) -> 
	{ok,Tokens,_EndLine} = erl_scan:string(Str),
	{ok,AbsForm} = erl_parse:parse_exprs(Tokens),
	{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
	Value.
	
% Envía el fichero ubicado en 'Filepath' a la máquina con 'Ip' al puerto 'Port'.
% La máquina destino debe usar 'file_receiver_loop' para recibir el fichero.
send_file(Ip, ReceivePort, FilePath)->
    {ok, Socket} = gen_udp:open(0, [binary, {active, once}]),
    {ok, FileBinary} = file:read_file(FilePath),
    util:send(Socket,Ip,ReceivePort,FileBinary),
    ok = gen_tcp:close(Socket).

% Recibe un fichero a través del puerto 'ReceivePort' y lo guarda con nombre
% 'Filename' en el directorio configurado
receive_file(Filename, Socket) ->
	inet:setopts(Socket, [{active,once}]),
    receive
    	{udp, Socket, _, _, Bin} ->
    		save_file(Filename, Bin)
    end.
    
% Guarda el contenido 'Bs' en un fichero de nombre Filename, en el directorio
% configurado.
save_file(Filename,Bs) ->
    {ok, Fd} = file:open(?KEYFOLDER ++ Filename ++ ".pub", write),
    file:write(Fd, Bs),
    file:close(Fd).