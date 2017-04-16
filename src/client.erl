% Autores:
%   - David Touriño Calvo
%   - Brais Muñiz Castro
% Módulo cliente:
%     Modela la funcionalidad a la que tiene acceso un nodo cliente normal.
-module(client).

-export([init/0, find_polls/3,poll_details/1,vote/2,new_poll/2,close_poll/1]).

-define(LOCAL_POLL_SERVER,local_poll_server).


% Debe ejecutarse al inicio siempre. Si hay encuestas activas arranca el
% "Poll Server".
init() -> case  dicc:polls_alive()  of
                true-> 
                        register(?LOCAL_POLL_SERVER,
                            spawn(server,init_poll_server,[]));
                false -> no_polls
          end.

% Pide al nodo Discover la lista de encuestas activas registradas.
% Params:
%   - IP:           Dirección IP del nodo discover al que se envía la petición.
%   - DiscoverPort: Puerto al que se envía la petición.
%   - Msg:          Mensaje que se envía.
% Returns:
%   - Lista de encuestas activas registradas.
%   - no_answer_from_server: Si el discover no responde.
find_polls(IP, DiscoverPort, Msg) ->
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

% Crea una nueva encuesta que sera hosteada por la propia máquina, y añade su
% fichero al directorio de encuestas acitvas. Si el proceso "Poll Server" no
% está ejecutándose, lo crea.
% Params:
%   - PollName:    Nombre de la encuesta.
%   - Description: Descripción de la encuesta.
% Returns:
%   -ok:          Todo ha salido correctamente.
new_poll(PollName,Description) ->
    case whereis(?LOCAL_POLL_SERVER) of
        undefined ->
            register(?LOCAL_POLL_SERVER,spawn(server,init_poll_server,[]));
        _ ->
            ok
    end,
    ?LOCAL_POLL_SERVER ! {new_poll,self(),PollName,Description},
    receive
        ReturnValue -> ReturnValue
    end.

% A partir de una tupla que identifique y localice una encuesta, pide al
% servidor encargado de esa encuesta los detalles de la misma.
% Param:
%   - Tupla id de encuesta.
% Returns:
%    -{     {poll_inf,
%                PollName,      Nombre identificador de la encuesta 
%                Description,   Descripcion de la encuesta
%                Limit          Fecha límite en milisegundos
%           },
%           {positive, N},      Número de votos positivos
%           {negative, M}       Número de votos negativos
%     }
poll_details({PollDir,PollPort,PollName}) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active,false}]),
    util:send(Socket,PollDir,PollPort,erlang:term_to_binary({poll_info,PollName})),
    inet:setopts(Socket,[{active,once}]),
    Result =  receive
                    {udp, Socket, PollDir, PollPort, Msg} ->
                        erlang:binary_to_term(Msg)
              end,
    util:close(Socket),
    Result.
    
% Permite al cliente votar una encuesta.
% Params:
%   - Tupla id. de encuesta.
%   - Option: Indica el valor "positive" o "negative" del voto (es un átomo).
% Returns:
%   - {error, already_voted}: El usuario ya ha votado anteriormente.
%   - {ok, voted}:            Cuando se actualiza sin errores.  
vote({PollDir,PollPort,PollName},Option) when Option == positive orelse Option == negative->
    {ok, Socket} = gen_udp:open(0, [binary, {active,false}]),
    util:send(Socket,PollDir,PollPort,erlang:term_to_binary({vote,PollName,Option})),
    inet:setopts(Socket,[{active,once}]),    
    Result =  receive
                    {udp, Socket, PollDir, PollPort, Msg} ->
                        erlang:binary_to_term(Msg)
              end,
    util:close(Socket),
    Result.

% Permite al cliente cerrar una encuesta activa.
% Params:
%   - PollName: Nombre de la encuesta.
% Returns:
%   {deleted, PollName}: La encuesta se ha borrado.
close_poll(PollName) ->
    case whereis(?LOCAL_POLL_SERVER) of
        undef ->
            no_active_local_polls;
        _ ->
            ?LOCAL_POLL_SERVER ! {close_poll, self(), PollName},
            receive
                {close,Result} -> Result
            end
    end.