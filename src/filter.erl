-module(filter).

-define(MAX, 4).

-export([filter/2, clear/1, update_list/2]).

filter(Ip, L) -> 
	case mylist:read(Ip,L) of
		{ok, N} when N =< ?MAX -> access_allowed;
		{error, instance} -> access_allowed;
		 _ -> access_denied
	end.

clear(Pid) ->
	timer:send_after(timer:seconds(60), Pid, vaciar_lista).

update_list(Ip, List) ->
	case mylist:read(Ip, List) of
		{error, instance} -> NewList = mylist:write(Ip, 1, List);
		{ok, Num} ->NewList= mylist:write(Ip, Num + 1,  mylist:delete(Ip, List))
	end,
	NewList.
