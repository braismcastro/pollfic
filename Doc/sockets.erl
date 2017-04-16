
%%%UDP:

%%PC1:
{ok, Socket} = gen_udp:open(8789, [binary, {active,true}]).

%%PC2:
{ok, Socket} = gen_udp:open(8790).
gen_udp:send(Socket, {127,0,0,1}, 8789, "hey there!").  %% Socket + IP + Port + Message

%%PC1:
flush().


%%%TCP:

%%PC1:
{ok, ListenSocket} = gen_tcp:listen(8091, [{active,true}, binary]).
{ok, AcceptSocket} = gen_tcp:accept(ListenSocket).

%%PC2:
{ok, Socket} = gen_tcp:connect({127,0,0,1}, 8091, [binary, {active,true}]).
gen_tcp:send(Socket, "Hey there first shell!").

%%PC1:
flush().


%%%%%%% CABECERA %%%% 