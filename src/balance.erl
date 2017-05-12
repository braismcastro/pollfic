-module(balance).

-export([start/0, loop/4]).

start() -> 
	BalancePort = dicc:get_conf(balance_port),
	{ok, Socket} = gen_udp:open(BalancePort, [binary, {active,true}]),
	io:format("Server opened socket:~p~n",[Socket]),
	spawn(filter, clear, [self()]),
	spawn(?MODULE, loop, [Socket, [], [], []]).


again(Socket, [H|[]], [], UserList) ->
	loop(Socket, [H], [], UserList);

again(Socket, [H|[]], RestList, UserList) ->
	loop(Socket, RestList, [H], UserList);
	
again(Socket, [H|T], RestList, UserList) ->
	loop(Socket, T, [H|RestList], UserList).


loop(Socket, [], [], UserList) ->	
    inet:setopts(Socket, [{active, once}]),
	receive
		{udp, Socket, IP, Port, Msg} ->
			case erlang:binary_to_term(Msg) of
				new_discover -> 
					loop(Socket, [{IP, Port}], [], UserList)
				_ -> util:send(Socket, IP, Port, erlang:term_to_binary(no_answer_from_server))
			end;
		vaciar_lista -> loop(Socket, [], [], [])		
	end;

loop(Socket, DiscList, RestList, UserList) ->
    inet:setopts(Socket, [{active, once}]),
	receive
		 {udp, Socket, IP, Port, Msg} ->
        	case filter:filter(IP, UserList) of
					access_allowed ->
						redirect(Socket, IP, Port, Msg, DiscList),
						case erlang:binary_to_term(Msg) of
							new_node -> 
								  util:send(Socket, IP, Port, erlang:term_to_binary(head(DiscList))),
								  again(Socket, [{IP, Port}|DiscList], RestList, UserList);
							_ ->  
								  again(Socket, DiscList, RestList, filter:filter(IP, UserList))
						end;
					access_denied -> 
						loop(Socket, DiscList, RestList, UserList)
			end;

		 vaciar_lista ->
				loop(Socket, DiscList, RestList, [])

		
	end.


redirect(Socket, Ip, Port, Msg, [{DiscoverIp, DiscoverPort}|_]) -> 
	Resend = erlang:binary_to_term(Msg), 
	util:send(Socket, DiscoverIp, DiscoverPort, 
		erlang:term_to_binary({Ip, Port, Resend})).

head([H|_]) -> H.