#!/usr/bin/env sh

#
# serv Init System
#
# @author: Henglin Li <henglinli@gmail.com>
#

cd `dirname $0`
case "${1:-''}" in
    'start')
	# start rel/serv
	echo "starting rel/serv ..."
	./rel/serv/bin/serv start
	;;

    'stop')
	echo "stopping rel/serv ..."
	./rel/serv/bin/serv stop
	killall -q epmd
	;;

    'attach')
	echo "attching rel/serv ..."
	./rel/serv/bin/serv attach
	;;

    'start-dev')
	echo "staring dev/dev* ..."
	for d in dev/dev*; do $d/bin/serv start; done
	;;

    'stop-dev')
	echo "stopping dev/dev* ..."
	for d in dev/dev*; do $d/bin/serv stop; done
	killall -q epmd
	;;

    'attach-dev')
	echo "attching rel/serv ..."
	./dev/dev1/bin/serv attach
	;;
    *)
	echo "serv Init System"
	echo "Usage: $SELF start[-dev]|stop[-dev]||attach[-dev]"
	exit 1
	;;
esac
