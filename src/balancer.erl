-module(balancer).

-export([start/0]).


%Inicializa el servidor con la configuración por defecto
%Lanza también el proceso durmiente que se encarga de avisar paa renovar la lista de baneos
start() -> 
	BalancerPort = dicc:get_conf(balancer_port),
	{ok, Socket} = gen_udp:open(BalancerPort, [binary, {active,true}]),
	io:format("Balancer started on port: ~p.~n",[BalancerPort]),
	spawn(filter, clear, [self()]),
	loop(Socket, [], []).

%Proceso bucle que atiende peticiones de cualquier nodo, y reenvia la peticion a un nodo discover
%Filtra las peticiones, y no atiende peticiones si se abusa del servicio.
loop(Socket, DiscList, UserList) ->
    inet:setopts(Socket, [{active, once}]),
	receive
		 {udp, Socket, IP, Port, Msg} ->
        	case filter:filter(IP, UserList) of
				access_allowed ->
					case erlang:binary_to_term(Msg) of
						new_discover ->
		     				io:format("New discover listed ~n"),
							loop(Socket, [{IP, Port}|DiscList], UserList);
						_ ->
							redirect(Socket, IP, Port, Msg, DiscList),	
							loop(Socket, toFinal(DiscList), filter:update_list(IP, UserList))
					end;
				access_denied -> 
					io:format("Rejected msg. User ~p has been banned. ~n",[IP]),
					loop(Socket, DiscList, UserList)
			end;
		 vaciar_lista ->
		 	io:format("Ban list reseted.~n"),
			loop(Socket, DiscList, [])
	end.

toFinal([])-> [];
toFinal([H|T]) -> T ++ [H].


%%Redirect:
%Renvía un mensaje recibido a la cabeza del discover de la lista
%Parámetros:
%	-Socket de la conexión
%	-Ip:	Ip origen de la peticion
%	-Port: 	Puerto origen de la peticion
%	-Msg: 	Mensaje a reenviar
%	-Lista  Con tuplas {Ip, Port} de cada discover.
redirect(_,_,_,_, []) ->
	io:format("Rejected petition due to empty discover list");

redirect(Socket, Ip, Port, Msg, [{DiscoverIp, DiscoverPort}|_]) -> 
	io:format("~p from ~p redirected to ~p ~n", [erlang:binary_to_term(Msg),{Ip, Port},{DiscoverIp, DiscoverPort}]),
	Resend = erlang:binary_to_term(Msg),
	util:send(Socket, DiscoverIp, DiscoverPort, 
		erlang:term_to_binary({Ip, Port, Resend})),
	util:send(Socket, Ip, Port, 
		erlang:term_to_binary({DiscoverIp, DiscoverPort})).