#!/bin/sh

n_num=$1
if [ $n_num -eq  ]; then
	n_num=1
fi

network_dst="192.168.$n_num."
for i in `seq 1 254`; do
	dst=$network_dst$i
	echo "ping to $dst"; res=`ping $dst -c 1` &
done
wait
echo "done"
