#!/bin/bash

START=$(sudo cat /sys/class/powercap/intel-rapl:0/energy_uj)
sleep 60  # <- your measurement window
END=$(sudo cat /sys/class/powercap/intel-rapl:0/energy_uj)

ENERGY_J=$(echo "scale=3; ($END - $START)/1000000" | bc)
echo "Energy used: $ENERGY_J Joules in 60 seconds"
