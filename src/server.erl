% Módulo server:
%   Modela la funcionalidad necesaria para hostear encuestas. Se divide en
%   dos partes: 
%     - Accesso al servicio: Ofrece diversas operaciones remotas y locales.
%     - Casos de uso: Implementa la funcionalidad que ofrece el acceso al
%           servicio.
-module(server).

-export([init_poll_server/0, are_polls_alive/0]).

-define(PUBLIC_KEY_PATH, "../keys/public_key.pem").
-define(PRIVATE_KEY_PATH, "../keys/private_key.pem").
-define(KEYPATH, "../keys/").

%%%%%%%%%%%%%% ACCESO AL SERVICIO %%%%%%%%%%%%%%%

% Abre el socket, carga la configuración y entra en el bucle.
% Ademas comprueba si hay encuestas, para enviarle al servicio discover
% para renovar el puerto de conexion 
init_poll_server() ->
    {ok,Socket}  = gen_udp:open(0, [binary,{active,true}]),
    BalancerIP  = dicc:get_conf(balancer_dir),
    BalancerPort = dicc:get_conf(balancer_port),
    case are_polls_alive()  of
        true-> 
            update_polls_port(Socket, BalancerIP, BalancerPort);
        _ -> 
            ok
    end,
    io:format("Poll server started on port: ~p ~n",[inet:port(Socket)]),
    poll_server_loop(Socket,BalancerIP,BalancerPort).


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
poll_server_loop(Socket, BalancerIP, BalancerPort) ->
    inet:setopts(Socket, [{active,once}]),
    receive
        {udp, Socket, From, FromPort, Msg} ->
            case erlang:binary_to_term(Msg) of
                vote ->
                    do_vote(Socket);

                {poll_info, PollName} -> 
                    PollInfo = info_poll(PollName),
                    util:send(Socket, From, FromPort, erlang:term_to_binary(PollInfo));

                Unknown ->
                     io:format("Recibido: ~p ~n", [Unknown])
            end;

        {new_poll,From,PollName,Description} ->
            From ! {register, register_in_node(Socket, BalancerIP, BalancerPort, PollName, Description)};
            
        {close_poll,From,PollName} ->
            %util:send(Socket, BalancerIP, BalancerPort, erlang:term_to_binary({delete,PollName})),
            From ! {close, close_poll(PollName)}
            
    end,
    poll_server_loop(Socket, BalancerIP, BalancerPort).
    
%Espera a recibir el voto encriptado, para después enviarle la respuesta sobre el voto
do_vote(Socket) ->
    inet:setopts(Socket,[{active,once}]),
    receive
        {udp, Socket, From, FromPort, EncryptedVote} ->
           {vote, PollName, Option} = encrypt:decrypt_with_priv(EncryptedVote),    
           VoteResult = vote(From, PollName, Option)
    end,
    util:send(Socket, From, FromPort, erlang:term_to_binary(VoteResult)).

% Actualiza el puerto que tiene el servidor de discover cuando el servidor
% se reconecta.
% Params:
%   - Socket:       Socket que se usará para envíar el mensaje de actualización
%   - DiscoverDir:  Tupla que representa la IP de Discover.
%   - DiscoverPort: Puerto en el que escucha Discover.
% Returns:
%   - port_changed: El puerto se ha actualizado en Discover correctamente.
%   - no_answer_from_server: Tras 10mil ms sin respuesta del servidor.
update_polls_port(Socket, BalancerIP, BalancerPort) ->
    util:send(Socket, BalancerIP, BalancerPort, erlang:term_to_binary(update_port)),
    {DiscoverIP,DiscoverPort} = util:receive_discover_inf(Socket, BalancerIP, BalancerPort),
    inet:setopts(Socket, [{active,once}]),
    receive
        {udp, Socket, DiscoverIP, DiscoverPort, Bin} ->
            erlang:binary_to_term(Bin)
    after 2000 ->
            no_answer_from_server
    end.

% Registra una encuesta en el nodo descubrimiento.
% Params:
%   - Socket:                Socket que se usará para enviar el mensaje a Discover.
%   - BalancerIP:            Tupla que representa la IP del servidor de balanceo.
%   - BalancerPort:          Puerto en el que escucha el servidor de balance.
%   - PollName:              Nombre de la encuesta que se registra.
%   - Description            Descripcion de la encuesta que se registra.
% Returns:
%   - registered:            Encuesta registrada en el nodo discover.
%   - name_not_avaliable:      Nombre de la encuesta ya utilizado.
%   - no_answer_from_server: Tras 10mil ms sin respuesta del servidor.
register_in_node(ServSocket, BalancerIP, BalancerPort, PollName, Description) ->
    {ok,Socket}  = gen_udp:open(0, [binary,{active,once}]),
    {ok, PollPort} = inet:port(ServSocket),
    util:send(Socket, BalancerIP, BalancerPort, erlang:term_to_binary({register,PollName, PollPort })),
    {DiscoverIP,DiscoverPort} = util:receive_discover_inf(Socket, BalancerIP, BalancerPort),
    util:send_file(DiscoverIP, DiscoverPort, ?PUBLIC_KEY_PATH),
    inet:setopts(Socket, [{active,once}]),
    receive
        {udp, Socket, DiscoverIP, DiscoverPort, Result} ->
            case erlang:binary_to_term(Result) of
                {ok,registered} ->
                    start_poll(PollName,Description),
                    registered;
                name_not_avaliable ->
                    name_not_avaliable
            end
    after 2000 ->
       no_answer_from_server
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CASOS DE USO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cierra una encuesta activa que coincida con el nombre 'PollName'. Las 
% encuestas cerradas se envían al directorio de encuestas cerradas.
% Params:
%   - PollName: Nombre de la encuesta que se va a cerrar.
% Returns:
%   {deleted, FileName}: Se ha cerrado satisfactoriamente la encuesta.
%   {error, not_found}:  Cuando no encuentra el archivo de la encuesta.
%    owner_error:        Se ha llamado a close con una encuesta que no posees
close_poll(PollName) -> 
    {ok,Socket}  = gen_udp:open(0, [binary,{active,once}]),
    BalancerIP = dicc:get_conf(balancer_dir),
    BalancerPort = dicc:get_conf(balancer_port),
    util:send(Socket, BalancerIP, BalancerPort, erlang:term_to_binary({delete,PollName})),
    {DiscoverIP,DiscoverPort} = util:receive_discover_inf(Socket, BalancerIP, BalancerPort),
    inet:setopts(Socket, [{active,once}]),
    receive 
        {udp, Socket, DiscoverIP, DiscoverPort, BinaryResponse} ->
            Result = erlang:binary_to_term(BinaryResponse),
            case Result of
                deleted -> dicc:close(PollName);
                owner_error -> owner_error;
                _ELSE -> true
            end,
            Result
    after 2000 ->
        no_answer_from_server
    end.


    
    
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
        _ -> 
            {error, alredy_voted}
    end.    