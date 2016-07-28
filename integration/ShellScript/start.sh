#!/bin/bash
echo start of Glob
# from url
if [ "$1" = "fromurl" ] && [ $# -eq 2 ]; then
  mkdir /etc/glob
  curl $2 > /etc/glob/config
  glob-launch -f /etc/glob/config +RTS -N -RTS
elif [ "$1" = "fromenv" ] && [ $# -eq 1 ]; then
  mkdir /etc/glob
  #
  glob-launch -f /etc/glob/config +RTS -N -RTS
elif [ "$1" = "fromenv" ] && [ "$2" = "help"]; then
  echo The list of
else
  echo err
  echo fromurl URL
  echo '    ' to get config file from url
  echo
  echo fromenv
  echo '    ' to get config from the environment variables
  echo
  echo fromenv help
  echo '    ' list the environment variables
  exit 1
fi
