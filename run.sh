#!/bin/bash
## USAGE: ./run.sh <RUN> <hostIp> <vegetation> <fire>

RUN=$1
hostIp=$2 #68 # Specify which machine this is running for
vegetation=$3 #"LandR.CS"
fire=$4 #"fS"

runSimulation="RUN <- '${RUN}'; hostIp <- '${hostIp}'; vegetation <- '${vegetation}'; fire <- '${fire}'; source('sourceUnattended.R')"

echo ${runSimulation} | r
