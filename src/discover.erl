-module(discover).

%% Public API
-export([start/0]).

-define(KEYFOLDER, "../keys/").

%%Inicializa el servidor discover en primer plano con la configuración por defecto
%%Solo admite mensajes del balanceador
start() ->
	DiscoverPort = dicc:get_conf(discover_port),
	{ok, Socket} = gen_udp:open(DiscoverPort, [binary, {active,true}]),
	io:format("Discover started on port: ~p ~n",[DiscoverPort]),
	BalancerIP   = dicc:get_conf(balancer_dir),
	BalancerPort = dicc:get_conf(balancer_port),
	% Nos registramos en el balancer.
	util:send(Socket, BalancerIP, BalancerPort, erlang:term_to_binary(new_discover)),
    loop(Socket,[],DiscoverPort, BalancerIP, BalancerPort).


%%Recibe del balanceador las peticiones de los clientes,
% con IP y puerto de los mismos, para responderles directamente
% Formato del mensaje udp: 	{IP Puerto, Mensaje}
%Formato del mensaje/Operaciones que acepta:
%	update_port:			Actualiza el puerto de las encuestas de la IP origen
%
%	poll_request:			Envía la lista de encuestas actuales
%
%	{register, PollName}:	Registra una nueva encuesta en el servicio
%
%	{delete, PollName}:		Borra una encuesta del servicio
%
%	{public_key, PollName}:	Devuelve la llave publica de una encuesta.
loop(Socket,PollList,FilePort, BalancerIP, BalancerPort) ->
    inet:setopts(Socket, [{active, once}]),
    io:format("Waiting for request on port ~n"),
    inet:setopts(Socket, [{active, once}]),

    receive
        {udp, Socket, BalancerIP, BalancerPort, BalancerMsg} ->
        	{IP,Port,Msg} = erlang:binary_to_term(BalancerMsg),
        	io:format("Request ~p from ~p . ~n",[Msg,IP]),
        	case Msg of	
        		update_port ->  
		        	NewPollList = update(PollList, IP, Port, []),
		        	util:send(Socket, IP, Port, erlang:term_to_binary(port_changed)),
		        	loop(Socket, NewPollList, FilePort, BalancerIP, BalancerPort);

            	poll_request -> 
            		util:send(Socket, IP, Port, erlang:term_to_binary(PollList)),
            		loop(Socket, PollList, FilePort, BalancerIP, BalancerPort);

            	{register, PollName, PollPort} ->   
            		register_poll(Socket, PollList, PollName, IP, Port, FilePort, BalancerIP, BalancerPort, PollPort);

		        {delete, PollName} -> 
		        	delete(Socket, PollList, PollName, IP, Port, FilePort, BalancerIP, BalancerPort);

                {public_key, PollName} -> 
                    util:send_file(IP, Port, ?KEYFOLDER++PollName++".pub"),
                    loop(Socket, PollList, FilePort, BalancerIP, BalancerPort)
        	end
    end.

    
%%register_poll
%Responde a un registro de crear encuesta a través de la red
%Returns (A través de la red):
%	{ok, registered}:	Se registra correctamente
%	name_not_avaliable:	El nombre no esta disponible, la encuesta no se registra
register_poll(Socket, PollList, PollName, IP, Port, FilePort, BalancerIP, BalancerPort, PollPort) ->
	case check_poll_name(PollList, PollName) of 
		true -> 
    		util:receive_file(PollName, Socket),
    		util:send(Socket,IP,Port,erlang:term_to_binary({ok,registered})),
    		loop(Socket, [{IP,PollPort,PollName}|PollList], FilePort, BalancerIP, BalancerPort);
    	_ ->  
    		util:send(Socket, IP, Port, erlang:term_to_binary(name_not_avaliable)),
    		loop(Socket, PollList, FilePort, BalancerIP, BalancerPort)
    end.


%delete
%Responde a una peticion de borrado de encuesta del servicio
%Returns (A través de la red):
%	deleted:			Se borra correctamente
%	owner_error:		El solicitante no es el propietario
%	non_existing_name:	No existe esa encuesta en el nodo
delete(Socket, PollList, PollName, IP, Port, FilePort, BalancerIP, BalancerPort) ->
	case check_poll_name(PollList,PollName) of 
		false -> 
			{PollIP,PollPort,PollName} = lists:keyfind(PollName,3,PollList),
			if IP == PollIP ->
				util:send(Socket, IP, Port, erlang:term_to_binary(deleted)),
				loop(Socket,lists:delete({PollIP,PollPort,PollName},PollList),FilePort, BalancerIP, BalancerPort);
			true -> 
				util:send(Socket, IP, Port, erlang:term_to_binary(owner_error)),
    		  	loop(Socket, PollList, FilePort, BalancerIP, BalancerPort)
    		end;
    	_ ->
    		util:send(Socket, IP, Port, erlang:term_to_binary(non_existing_name)),
    		loop(Socket, PollList, FilePort, BalancerIP, BalancerPort)
    end.


%%Comprueba si no existe un nombre de encuesta en la lista
%%returns: 
%		true:	No existe
%		false:	Existe
check_poll_name([], _) -> true;

check_poll_name([{_,_,PollName}|_], PollName) -> false;

check_poll_name([_|T], PollName) -> 
	check_poll_name(T, PollName).

%Actualiza la lista de polls de una ip con un nuevo puerto
update([], _, _, Acc) -> Acc;

update([{IP,_,PollName}|T], IP, Port, Acc) -> 
	update(T, IP, Port, [{IP,Port,PollName}|Acc]);

update([H|T], IP, Port, Acc) ->
	update(T, IP, Port, [H|Acc]).