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

-export([init_poll_server/0, are_polls_alive/0]).

%%%%%%%%%%%%%% ACCESO AL SERVICIO %%%%%%%%%%%%%%%

% Abre el socket, carga la configuración y entra en el bucle.
init_poll_server() ->
    {ok,Socket}  = gen_udp:open(0, [binary,{active,true}]),
    DiscoverDir  = dicc:get_conf(discover_dir),
    DiscoverPort = dicc:get_conf(discover_port),
    update_polls_port(Socket,DiscoverDir,DiscoverPort),  % Comprobar que ha funcionado o si no volver a intentarlo en bucle
    poll_server_loop(Socket,DiscoverDir,DiscoverPort).


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
poll_server_loop(Socket, DiscoverDir, DiscoverPort) ->
    inet:setopts(Socket, [{active,once}]),
    receive
        {udp, Socket, From, FromPort, Msg} ->
            case erlang:binary_to_term(Msg) of
                {vote, PollName, Option} ->
                    VoteResult = vote(From,PollName,Option),
                    util:send(Socket, From, FromPort, erlang:term_to_binary(VoteResult));
                {poll_info, PollName} -> 
                    PollInfo = info_poll(PollName),
                    util:send(Socket, From, FromPort, erlang:term_to_binary(PollInfo));
                Msg -> io:format("Recibido: ~p", [Msg])
            end;
        {new_poll,From,PollName,Description} ->
            From ! register_in_node(Socket, DiscoverDir, DiscoverPort, PollName, Description);
        {close_poll,From,PollName} ->
            % FALTA: BUCLE COMPROBANDO QUE EL DISCOVER LO HA BORRADO.
            util:send(Socket,  DiscoverDir, DiscoverPort, erlang:term_to_binary({delete, PollName})), 
            From ! {close, close_poll(PollName)}
            
    end,
    %poll_server_loop(Socket, DiscoverDir, DiscoverPort).
    check_polls(are_polls_alive(), Socket, DiscoverDir, DiscoverPort).
    
check_polls(true, Socket, DiscoverDir, DiscoverPort) -> 
    poll_server_loop(Socket, DiscoverDir, DiscoverPort);
check_polls(false, _, _, _) -> 
    exit(normal).


% Actualiza el puerto que tiene el servidor de discover cuando el servidor
% se reconecta.
% Params:
%   - Socket:       Socket que se usará para envíar el mensaje de actualización
%   - DiscoverDir:  Tupla que representa la IP de Discover.
%   - DiscoverPort: Puerto en el que escucha Discover.
% Returns:
%   - port_changed: El puerto se ha actualizado en Discover correctamente.
%   - no_answer_from_server: Tras 10mil ms sin respuesta del servidor.
update_polls_port(Socket, DiscoverDir, DiscoverPort) ->
    util:send(Socket, DiscoverDir, DiscoverPort, erlang:term_to_binary({renew})),
    receive
        {udp, Socket, _, _, Bin} ->
            erlang:binary_to_term(Bin)
    after 10000 ->
            no_answer_from_server
    end.


% Registra una encuesta en el nodo descubrimiento.
% Params:
%   - Socket:                Socket que se usará para enviar el mensaje a Discover.
%   - DiscoverDir:           Tupla que representa la IP de Discover.
%   - DiscoverPort:          Puerto en el que escucha Discover.
%   - PollName:              Nombre de la encuesta que se registra.
% Returns:
%   - registered:            Encuesta registrada en el nodo discover.
%   - name_not_aviable:      Nombre de la encuesta ya utilizado.
%   - no_answer_from_server: Tras 10mil ms sin respuesta del servidor.
register_in_node(Socket, DiscoverDir, DiscoverPort, PollName, Description) ->
    util:send(Socket, DiscoverDir, DiscoverPort, erlang:term_to_binary({register,PollName})),
    receive
        {udp, Socket, _, _, Bin} ->
            case erlang:binary_to_term(Bin) of
                registered -> 
                    start_poll(PollName,Description),
                    registered;
                Response -> 
                    Response
            end
    after 10000 ->
            no_answer_from_server
    end.

%%%%%%%%%%%%%%%%%% CASOS DE USO %%%%%%%%%%%%%%%%%


% Cierra una encuesta activa que coincida con el nombre 'PollName'. Las 
% encuestas cerradas se envían al directorio de encuestas cerradas.
% Params:
%   - PollName: Nombre de la encuesta que se va a cerrar.
% Returns:
%   {deleted, FileName}: Se ha cerrado satisfactoriamente la encuesta.
%   {error, not_found}:  Cuando no encuentra el archivo de la encuesta.
close_poll(PollName) -> 
    dicc:close(PollName).
    
    
% Comprueba si hay alguna encuesta activa.
% Returns:
%   - true: Cuando hay al menos una encuesta activa.
%   - false: Cuando no hay ninguna encuesta activa.
are_polls_alive() -> 
        {ok, List} = dicc:get_file_list(),
        is_empty(List).

is_empty([]) -> false;
is_empty(_) -> true.


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