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
-export([start/2]).


%%--------------------------------------------------------------------
%% @doc Function 'start'
%% @spec start(	Value1 :: Port)
%% @end
%% @doc Start discover node listening service in the giving port.
%% @end
%%--------------------------------------------------------------------

start(Port,L) ->
    spawn(fun() -> init(Port,L) end).


init(Port,L) ->
	{ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
	io:format("Server opened socket:~p~n",[Socket]),
	loop(Socket,L).


loop(Socket,L) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, IP, Port, Msg} ->
            io:format("Server received:~p~n",[Msg]),
            case erlang:binary_to_term(Msg) of
            	poll_request -> util:send(Socket, IP, Port, erlang:term_to_binary(L)),
            					loop(Socket, L);
            	{register, PollName} ->   case check_poll_name(L,PollName) of 
            								true -> 
		            							  util:send(Socket, IP, Port, erlang:term_to_binary(registered)),
		            							  loop(Socket, [{IP,Port,PollName}|L]);
		            						_ ->  util:send(Socket, IP, Port, erlang:term_to_binary(name_not_avaliable)),
		            							  loop(Socket, L)
		            					  end;

		        {delete, PollName} -> case check_poll_name(L,PollName) of 
		        						false -> 
		        							{PollIP,PollPort,PollName} = lists:keyfind(PollName,3,L),
		        							if IP == PollIP ->
		        									util:send(Socket, IP, Port, erlang:term_to_binary(deleted)),
		        									loop(Socket,lists:delete({PollIP,PollPort,PollName},L));
		        								true -> 
		        									util:send(Socket, IP, Port, erlang:term_to_binary(owner_error)),
		            							  	loop(Socket, L)
		            						end;
		            					_ ->
		            						util:send(Socket, IP, Port, erlang:term_to_binary(non_existing_name)),
		            						loop(Socket, L)
		            				  end;

		        {renew} ->  NewL = update(L, IP, Port, []),
		        			util:send(Socket, IP, Port, erlang:term_to_binary(port_changed)),
		        			loop(Socket, NewL)

            end
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

