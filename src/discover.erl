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
-export([start/2, start/0]).

-define(KEYFOLDER, "../keys/").


%%--------------------------------------------------------------------
%% @doc Function 'start'
%% @spec start(	Value1 :: Port)
%% @end
%% @doc Start discover node listening service in the giving port.
%% @end
%%--------------------------------------------------------------------

start(Port,L) ->
    spawn(fun() -> init(Port,L) end).

start()->
	DiscoverPort = dicc:get_conf(discover_port),
	spawn(fun()-> init(DiscoverPort, []) end).

init(Port,L) ->
	{ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
	io:format("Server opened socket:~p~n",[Socket]),
	loop(Socket,L, Port, []).


loop(Socket,L, FilePort, IpList) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, IP, Port, Msg} ->
        	case filter:filter(IP, IpList) of
					access_allowed ->
						io:format("Access allowed",[]),
		            	case erlang:binary_to_term(Msg) of	
			            	poll_request -> util:send(Socket, IP, Port, erlang:term_to_binary(L)),
			            					loop(Socket, L, FilePort, filter:update_list(IP, IpList));
			            	{register, PollName} ->   case check_poll_name(L, PollName) of 
			            								true -> 
					            							util:receive_file(PollName, Socket),
					            							util:send(Socket,IP,Port,erlang:term_to_binary({ok,registered})),
					            							loop(Socket, [{IP,Port,PollName}|L], FilePort, filter:update_list(IP, IpList));
					            						_ ->  
					            							util:send(Socket, IP, Port, erlang:term_to_binary(name_not_avaliable)),
					            							loop(Socket, L, FilePort, filter:update_list(IP, IpList))
					            					  end;
					        {delete, PollName} -> case check_poll_name(L,PollName) of 
					        						false -> 
					        							{PollIP,PollPort,PollName} = lists:keyfind(PollName,3,L),
					        							if IP == PollIP ->
					        									util:send(Socket, IP, Port, erlang:term_to_binary(deleted)),
					        									loop(Socket,lists:delete({PollIP,PollPort,PollName},L),FilePort,  filter:update_list(IP, IpList));
					        								true -> 
					        									util:send(Socket, IP, Port, erlang:term_to_binary(owner_error)),
					            							  	loop(Socket, L, FilePort,  filter:update_list(IP, IpList))
					            						end;
					            					_ ->
					            						util:send(Socket, IP, Port, erlang:term_to_binary(non_existing_name)),
					            						loop(Socket, L, FilePort,  filter:update_list(IP, IpList))
					            				  end;
			                {public_key, PollName} -> 
			                            util:send_file(IP, Port, ?KEYFOLDER++PollName++".pub"),
			                            loop(Socket, L, FilePort,  filter:update_list(IP, IpList));
					        {renew} ->  NewL = update(L, IP, Port, []),
					        			util:send(Socket, IP, Port, erlang:term_to_binary(port_changed)),
					        			loop(Socket, NewL, FilePort,  filter:update_list(IP, IpList))
		            	end;
		            access_denied ->io:format("Access denied",[]), loop(Socket, L, FilePort, filter:update_list(IP, IpList))
		    end;
		{vaciar_lista} ->
				loop(Socket,L, FilePort, [])
    end.


check_poll_name([], _) -> true;

check_poll_name([{_,_,PollName}|_], PollName) -> false;

check_poll_name([_|T], PollName) -> 
	check_poll_name(T, PollName).


update([], _, _, Acc) -> Acc;

update([{IP,_,PollName}|T], IP, Port, Acc) -> 
	update(T, IP, Port, [{IP,Port,PollName}|Acc]);

update([H|T], IP, Port, Acc) ->
	update(T, IP, Port, [H|Acc]).
