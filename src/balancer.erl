-module(balancer).

-export([start/0]).

start() -> 
	BalancerPort = dicc:get_conf(balancer_port),
	{ok, Socket} = gen_udp:open(BalancerPort, [binary, {active,true}]),
	io:format("Balancer started on port: ~p.~n",[BalancerPort]),
	spawn(filter, clear, [self()]),
	loop(Socket, [], []).



loop(Socket, [], UserList) ->	
    inet:setopts(Socket, [{active, once}]),
	receive
		{udp, Socket, IP, Port, Msg} ->
			case erlang:binary_to_term(Msg) of
				new_discover -> 
					io:format("New discover listed ~n"),
					loop(Socket, [{IP, Port}], UserList);
				_ -> 
					io:format("Rejected msg due to empty discover list. ~n"),
					util:send(Socket, IP, Port, erlang:term_to_binary(no_answer_from_server))
			end;
		vaciar_lista ->
			io:format("Ban list reseted.~n"),
			loop(Socket, [], [])		
	end;

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
							io:format("~p from ~p redirected to ~p ~n", [erlang:binary_to_term(Msg),{IP},head(DiscList)]),
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

redirect(Socket, Ip, Port, Msg, [{DiscoverIp, DiscoverPort}|_]) -> 
	Resend = erlang:binary_to_term(Msg),
	util:send(Socket, DiscoverIp, DiscoverPort, 
		erlang:term_to_binary({Ip, Port, Resend})),
	util:send(Socket, Ip, Port, 
		erlang:term_to_binary({DiscoverIp, DiscoverPort})).

head([H|T]) -> H.