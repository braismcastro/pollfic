rm -r `dirname $0`/bin
mkdir `dirname $0`/bin
mkdir `dirname $0`/bin/polls
mkdir `dirname $0`/bin/oldpolls
erlc -o `dirname $0`/bin/ `dirname $0`/src/server.erl `dirname $0`/src/discover.erl `dirname $0`/src/util.erl `dirname $0`/src/dicc.erl `dirname $0`/src/gui.erl `dirname $0`/src/client.erl

