%%Modulo de filtrado para gestionar la lista e usuarios
%Si se supera MAX mensajes en SLEEP milisegundos
%La operacion filter, devolvera acces_denied


-module(filter).

-define(MAX, 20).
-define(SLEEP, 60000).

-export([filter/2, clear/1, update_list/2]).

%Devuelve access_denied si en la lista aparecen MAX ocurrencias de la IP
%Devuelve access_allowed en caso contrario
filter(Ip, L) -> 
	case mylist:read(Ip,L) of
		{ok, N} when N =< ?MAX -> access_allowed;
		{error, instance} -> access_allowed;
		 _ -> access_denied
	end.
%%Duerme y cada SLEEP milisegundos envia un mensaje a Pid para vaciar la lista
clear(Pid) ->
    timer:sleep(?SLEEP),
    Pid ! vaciar_lista,
    clear(Pid).

%Actualiza la lista añadiendo una ocurrencia de la IP a la lista
update_list(Ip, List) ->
	case mylist:read(Ip, List) of
		{error, instance} -> NewList = mylist:write(Ip, 1, List);
		{ok, Num} ->NewList= mylist:write(Ip, Num + 1,  mylist:delete(Ip, List))
	end,
	NewList.
