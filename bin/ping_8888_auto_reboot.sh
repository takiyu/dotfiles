#!/bin/sh

while true ; do
    sleep 300
    ping -c 1 8.8.8.8
    if [ $? -ne 0 ] ; then
        reboot  # Assuming super user
    else
        echo success
    fi
done
