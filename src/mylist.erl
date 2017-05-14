%Modulo auxiliar para trabajar operaciones con listas

-module(mylist).

-export([new/0, write/3, delete/2, read/2, match/2, destroy/1]).


%new() -> DbRef Crea el almacen de datos.

new() -> [].

%write(Key, Element, DbRef) -> NewDbRef Inserta un nuevo elemento en el almacen DbRef.
write(Key, Element, DbRef) -> [{Key,Element}|DbRef].




%delete(Key, DbRef) -> NewDbRef Elimina la primera ocurrencia de la clave Key en el almacen DbRef.
delete(Key, DbRef) -> delete_aux(Key, DbRef, []).

concat([], L) -> L;
concat([H|T], L) -> concat(T, [H|L]).

delete_aux(_,[],NewDbRef) -> NewDbRef;
delete_aux(Key, [H|T], NewDbRef) -> 
	case H of 
		{Key,_} -> concat(NewDbRef, T);
		_ -> delete_aux(Key, T, [H|NewDbRef])
		
	end.
	
%read(Key, DbRef) -> {ok, Element} | {error, instance} Recupera la primera ocurrencia de la clave Key en el almacen DbRef, o devuelve un valor de error sin no existe.
read(_, []) -> {error, instance};
read(Key, [{Key, Element}|_]) -> {ok, Element};
read(Key, [_|T]) -> read(Key, T).

%match(Element, DbRef) -> [Key, ..., KeyN] Recupera todas las claves que contengan el valor Element.
match(Element, DbRef) -> [Key || {Key,X} <- DbRef, X==Element].

%destroy(DbRef) -> ok Elimina el almacenamiento DbRef.
destroy(_) -> ok.
