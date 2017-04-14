%%%-------------------------------------------------------------------
%%% @author Daniel Gabín.
%%% @copyright (C) 2017.
%%% @doc Practica de AS: Servicio de votaciones.
%%% Cliente de servicio de votaciones - Funcionalidad descubrir votaciones.
%%% @end
%%% Created : 2017 by the authors.
%%%-------------------------------------------------------------------
 
-module(client).

%% Public API
-export([findPoll/3]).

%%--------------------------------------------------------------------
%% @doc Function 'findPoll'
%% @spec findPoll(	Value1 :: IP
%%					Value2 :: Port
%%					Value3 :: Msg) -> Msg | Error
%% @end
%% @doc Returns list of active poll servers or an error if server doesn't
%%	    answer after 10 seconds.
%% @end
%%--------------------------------------------------------------------

findPoll(IP, DiscoverPort, Msg) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("Client opened socket=~p~n",[Socket]),
	util:send(Socket, IP, DiscoverPort, Msg),
    Value = receive
                {udp, Socket, _, _, Bin} ->
                    io:format("Client received:~p~n",[Bin])
            after 10000 ->
                    no_answer_from_server
            end,
    gen_udp:close(Socket),
    Value.


