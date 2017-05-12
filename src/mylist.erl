-module(mylist).

-export([new/0, write/3, delete/2, read/2, match/2, destroy/1]).



new() -> [].


write(Key, Element, DbRef) -> [{Key,Element}|DbRef].


concat([], L) -> L;
concat([H|T], L) -> concat(T, [H|L]).

delete(Key, DbRef) -> delete_aux(Key, DbRef, []).

delete_aux(_,[],NewDbRef) -> NewDbRef;
delete_aux(Key, [H|T], NewDbRef) -> 
	case H of 
		{Key,_} -> concat(NewDbRef, T);
		_ -> delete_aux(Key, T, [H|NewDbRef])
		
	end.
	

read(_, []) -> {error, instance};
read(Key, [{Key, Element}|_]) -> {ok, Element};
read(Key, [_|T]) -> read(Key, T).


match(Element, DbRef) -> [Key || {Key,X} <- DbRef, X==Element].


destroy(_) -> ok.
