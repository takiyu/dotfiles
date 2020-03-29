#!/bin/bash

echo 'sudo cp *.rules /etc/udev/rules.d/'
sudo cp *.rules /etc/udev/rules.d/

echo 'sudo udevadm control --reload'
sudo udevadm control --reload
