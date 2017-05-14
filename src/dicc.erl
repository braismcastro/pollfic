% Modulo diccionario que guarda pares clave valor
% EL diccionario es almacenado como un fichero con la siguiente estructura:
% {Clave, Valor}
% {Clave2, Valor}
% {Clave3, Valor}
% Siendo tanto el valor como la clave cualquier término de erlang.
% Las operaciones reciben un nombre, FileName, que es el nombre de la 
% encuesta/nombre del fichero
% Reciben y emiten expresiones de erlang
-module(dicc).
-export([add/3, update/3, get/2, close/1, get_file_list/0, get_conf/1]).
-define(GPATH,"./polls/").
-define(OPATH, "./oldpolls/").
-define(CONF, "../config").

% Devuelve la tupla {Key, Value} para una clave y un fichero que le mandes
get(FileName, Key) ->
    {ok, File} = file:open(?GPATH++FileName, [read]),
    Value = get_aux(File, Key),
    file:close(File),
    Value.
    
%Devuelve el valor de una tupla del archivo de configuración
get_conf(Param) -> 
    {ok, File} = file:open(?CONF, [read]),
    {_, Value} = get_aux(File, Param),
    file:close(File),
    Value.

% Función recursiva auxiliar de get, para cada linea, busca la key que se 
% pasa por parámetro, y devuelve la tupla, si llega al final del fichero, devuelve error.
get_aux(File, Key) ->
    Line = io:get_line(File, []),
    case util:str_to_term(Line) of 
        eof -> {error, not_found};
        {Key, Something} -> {Key, Something};
        _ -> get_aux(File, Key)
    end.

% Actualiza para un fichero "FileName" una tupla con la llave "Key", 
% dándole un nuevo valor "Value"
update(FileName, Key, Value) ->
    {ok, File} = file:open(?GPATH++FileName, [read, write]),
    update_aux(File, Key, Value),
    file:close(File).

update_aux(File, Key, Value) ->
    {ok, Pos} = file:position(File, cur), %%Guarda la posicion de la linea actual
    Line = io:get_line(File, []),
    case util:str_to_term(Line) of 
        {Key, _} -> file:position(File,Pos),  %%Vuelve a la posicion de antes para escribir encima
                    %%Sobre escribe la linea actual a blancos
                    override(File, length(Line)), 
                    file:position(File,Pos),
                    file:write(File, util:term_to_str({Key,Value}));
        eof -> {error, not_found};      
        _ -> update_aux(File, Key, Value)
    end.

% Añade al final del fichero el par {Key, Value}, si no existe el fichero, lo crea.
add(FileName, Key, Value) ->
    {ok, File} = file:open(?GPATH++FileName, [append]),
    %file:write(File, util:term_to_str({Key, Value})++"\n"),
    %io:fwrite(File, "{~p, ~p}.\n", [Key, Value]),
    io:write(File, {Key, Value}), %Esta nueva version arregla el error de las descripiones largas
    io:fwrite(File, ".~n", []),
    file:close(File).


% Mueve un fichero del directorio de encuestas activas "polls/" al de inactivas
% "oldpolls/"
close(FileName) -> 
    file:copy(?GPATH++FileName, ?OPATH++FileName),
    file:delete(?GPATH++FileName),
    {deleted, FileName}.

% Devuelve true si hay encuestas activas y false en caso contrario.
get_file_list() -> 
    file:list_dir_all(?GPATH).

% Función auxiliar para sobreescribir una numero de caracteres a blancos
override(_, 1) -> ok;
override(File, Chars) when Chars>0->
    file:write(File, " "),
    override(File, Chars-1).