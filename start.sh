#!/bin/bash
if [[ $EUID -ne 0 ]]; then
  echo "You must be a root user" 2>&1
  exit 1
else
  export LD_LIBRARY_PATH=/usr/local/lib
  /home/jurrien/.cabal/bin/jcu -p 80 &
fi
