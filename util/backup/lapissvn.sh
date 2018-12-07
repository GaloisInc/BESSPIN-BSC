#! /bin/bash
set -e
time=`date --iso=hours`
cd /raid
7zr a -t7z -mx=9 /home/backup/backup-lapisvn/lapisvn-$time.7z lapisvn > /dev/null
#106 minutes 5.55 GB becomes 871 MB
