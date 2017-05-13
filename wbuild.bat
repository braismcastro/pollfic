RD "./bin" /S /Q 
mkdir bin
mkdir "./bin/polls"
mkdir "./bin/oldpolls"
erlc -o ./bin/ ./src/balancer.erl ./src/mylist.erl ./src/filter.erl ./src/server.erl ./src/discover.erl ./src/util.erl ./src/dicc.erl ./src/gui.erl ./src/client.erl ./src/encrypt.erl