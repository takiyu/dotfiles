#!/bin/sh
sudo nvidia-smi -i 0 -pm 1
sudo nvidia-smi -i 1 -pm 1
sudo nvidia-smi -i 2 -pm 1
sudo nvidia-smi -i 3 -pm 1

sudo nvidia-smi -i 0 -pl 120
sudo nvidia-smi -i 1 -pl 120
sudo nvidia-smi -i 2 -pl 120
sudo nvidia-smi -i 3 -pl 160
