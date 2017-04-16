% Autores:
%   - David Touriño Calvo
%   - Brais Muñiz Castro

% Módulo server:
%   Modela la funcionalidad necesaria para hostear encuestas. Se divide en
%   dos partes: 
%     - Accesso al servicio: Ofrece diversas operaciones remotas y locales.
%     - Casos de uso: Implementa la funcionalidad que ofrece el acceso al
%           servicio.
-module(server).

-export([init_poll_server/0]).
% HAY QUE MIRAR COMO IDENTIFICAMOS AL NODO DISCOVER
-define(DISCOVER,{0,0,0,0}).



%%%%%%%%%%%%%% ACCESO AL SERVICIO %%%%%%%%%%%%%%%

% Abre el socket y entra en el bucle.
init_poll_server() ->
    {ok,Socket} = gen_udp:open(0, [binary,{active,true}]),
    poll_server_loop(Socket).

% Se queda a la escucha de los mensajes que el servidor pueda recibir (locales
% y remotos) y los procesa. Si no hay encuestas activas el servidor aborta la
% ejecución.
% procesa.
% Operaciones que atiende:
%   Remotas:
%   - vote: Registrar y añadir un voto a la opción que se indique. Envía
%           confirmación o notificación de error al socket del cual provenga el
%           voto.
%   - poll_info: Envía la información acerca de la votación al socket del cual
%                proviene la petición.
%   Locales:
%   - new_poll:   Crea una nueva encuesta para hosterarla y la registra en 
%                 Discover.
%   - close_poll: Cierra una encuesta activa y la borra en Discover.
poll_server_loop(Socket) ->
    inet:setopts(Socket, [{active,once}]),
    receive
        {udp, Socket, From, FromPort, Msg} ->
            case erlang:binary_to_term(Msg) of
                {vote, PollName, Option} ->
                    VoteResult = vote(From,PollName,Option),
                    util:send(Socket, From, FromPort, erlang:term_to_binary(VoteResult));
                {poll_info, PollName} -> 
                    PollInfo = info_poll(PollName),
                    util:send(Socket, From, FromPort, erlang:term_to_binary(PollInfo))
            end;
        {new_poll,From,PollName,Description} ->
            % FALTA: Comprobar el poll name está disponible en discover.
            start_poll(PollName,Description),
            From ! ok;
        {close_poll,From,PollName} ->
            % FALTA: Enviar la petición de borrado al discover.
            From ! {close, dicc:close(PollName)}
    end,
    check_polls(dicc:polls_alive(),Socket).

    
check_polls(true,Socket) -> poll_server_loop(Socket);
check_polls(false,_) -> exit(normal).



%%%%%%%%%%%%%%%%%% CASOS DE USO %%%%%%%%%%%%%%%%%

% Crea una encuesta. Genera un fichero de encuesta en el directorio de
% encuestas activas y lo inicializa.
% Params: 
%   - Name:        Nombre identificador de la encuesta.
%   - Description: Descripción de la encuesta.
% Returns:
%   -ok:          Todo ha salido correctamente.
start_poll(PollName, Description) -> 
        dicc:add(PollName, poll_inf, {PollName, Description}),
        dicc:add(PollName, positive, 0),
        dicc:add(PollName, negative, 0).
        
% Devuelve la informacion de una encuesta.
% Params
%   - PollName:                 Nombre identificador de la encuesta
% Returns:
%    -{     {poll_inf,
%                PollName,      Nombre identificador de la encuesta 
%                Description,   Descripcion de la encuesta
%                Limit          Fecha límite en milisegundos
%           },
%           {positive, N},      Número de votos positivos
%           {negative, M}       Número de votos negativos
%     }
info_poll(PollName) ->
        {dicc:get(PollName, poll_inf), 
        dicc:get(PollName, positive), 
        dicc:get(PollName, negative)}.

% Añade un voto a la opción de la encuesta que se indique por parámetro.
% Además registra que "Who" ya ha votado en esta encuesta.
% Params:
%   - Who:     Identifica quién emite el voto.
%   - PollName: Nombre de la encuesta (y del fichero de votación)
%   - Option:   Identifica la opción que se está votando.
% Returns:
%   - {error, already_voted}: El usuario ya ha votado anteriormente.
%   - {ok, voted}:            Cuando se actualiza sin errores.   
vote(Who, PollName, Option) -> 
    case dicc:get(PollName, Who) of 
        {error, not_found} -> 
            {Option, Votes} = dicc:get(PollName, Option),
            dicc:update(PollName, Option, Votes+1),
            dicc:add(PollName, Who, voted),
            {ok, voted};
        _ -> {error, alredy_voted}
    end.    