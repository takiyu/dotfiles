#!/bin/sh

while true ; do
    ping -c 1 8.8.8.8
    if [ $? -ne 0 ] ; then
        notify-send "Failed to Ping"
    else
        echo success
    fi
    sleep 5
done
