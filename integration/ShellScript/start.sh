#!/bin/bash
echo start of Glob
# from url
if [ "$1" = "fromurl" ] && [ $# -eq 2 ]; then
  mkdir /etc/glob
  curl $2 > /etc/glob/config
  if [ -z "$(printenv $(cat /etc/glob/config | jq '."password-enviroment-variable"' -r))" ]; then
    echo -e "\a"
    echo "WARNING the environment variables where hold password is empty, of null. You might change the database without password or can not upload things to database."
  fi
  glob-launch -f /etc/glob/config +RTS -N -RTS
elif [ "$1" = "fromenv" ] && [ $# -eq 1 ]; then
  mkdir /etc/glob
  # default
  if [ -z "$GLOB_PORT" ];then
    export GLOB_PORT="3000"
    echo 'using default of port (3000)'
  fi
  if [ -z "$GLOB_DB_ADDR" ];then
    export GLOB_DB_ADDR="localhost:27017"
    echo 'using default of database address (localhost:27017)'
  fi
  if [ -z "$GLOB_DB_NAME" ];then
    export GLOB_DB_NAME="local"
    echo 'using default of database name (local)'
  fi
  if [ -z "$GLOB_DB_AM" ];then
    export GLOB_DB_AM="master"
    echo 'using default of database access mode (master)'
  fi
  if [ -z "$GLOB_DB_USR" ];then
    export GLOB_DB_USR=
    echo 'using nothing as the default of user'
  fi
  if [ -z "$GLOB_DB_PK" ];then
    export GLOB_DB_PK=
    echo 'using nothing as the default of user password'
  fi
  if [ -z "$GLOB_DB_PS" ];then
    export GLOB_DB_PS=10
    echo "using default of database's connection's  stripes(distinct sub-pools,which to maintain)'s number (10)"
  fi
  if [ -z "$GLOB_DB_PK" ];then
    export GLOB_DB_PK=100
    echo "using default of time of database's connection kept on (100(s))"
  fi
  if [ -z "$GLOB_DB_PSM" ];then
    export GLOB_DB_PSM=10
    echo "using default of the maximum number of database's connection's stripes (10)"
  fi
  if [ -z "$GLOB_TITLE" ];then
    export GLOB_TITLE="Glob"
    echo "using default of the site's title (Glob)"
  fi
  if [ -z "$GLOB_PSK_ENV_VAR" ];then
    export GLOB_PSK_ENV_VAR="GLOB_PSK"
    echo "using default of the site's password's environment variables (GLOB_PSK)"
  fi
  if [ -z "$GLOB_LOG_PATH" ];then
    export GLOB_LOG_PATH="stdout"
    echo "using default of log-files' path (stdout, output to stdout)"
  fi
  if [ -z "$GLOB_TYPE" ];then
    export GLOB_TYPE="*"
    echo "using default of listen type (*,at both ipv4 and ipv6 of each ip)"
  fi
  # check password environment variables
  if [ -z "$(printenv $GLOB_PSK_ENV_VAR)" ]; then
    echo -e "\a"
    echo "WARNING the environment variables where hold password is empty, of null. You might change the database without password or can not upload things to database."
  fi
  # write to /etc/glob/config
  echo "{ \"port\" : \"$GLOB_PORT\" ">/etc/glob/config
  echo ", \"database\" :" >> /etc/glob/config
  echo "\t{ \"hostaddr\" : \"$GLOB_DB_ADDR\" ">> /etc/glob/config
  echo "\t, \"database\" : \"$GLOB_DB_NAME\" ">> /etc/glob/config
  echo "\t, \"access-mode\": \"$GLOB_DB_AM\" ">> /etc/glob/config
  echo "\t, \"username\" : \"$GLOB_DB_USR\" ">> /etc/glob/config
  echo "\t, \"pool-stripes\" : \"$GLOB_DB_PS\" ">> /etc/glob/config
  echo "\t, \"pool-kept-time\" : \"$GLOB_DB_PK\" ">> /etc/glob/config
  echo "\t, \"pool-strp-max\" : \"$GLOB_DB_PSM\" ">> /etc/glob/config
  echo "\t}" >> /etc/glob/config
  echo ", \"title\" : \"$GLOB_TITLE\" ">> /etc/glob/config
  echo ", \"password-environment-variable\" : \"$GLOB_PSK_ENV_VAR\" ">> /etc/glob/config
  echo ", \"log-path\" : \"$GLOB_LOG_PATH\" ">> /etc/glob/config
  echo ", \"listen-type\" : \"$GLOB_TYPE\" ">> /etc/glob/config
  echo "}" >> /etc/glob/config
  #
  glob-launch -f /etc/glob/config +RTS -N -RTS
elif [ "$1" = "fromenv" ] && [ "$2" = "help"]; then
  echo The list of environment.
else
  echo err
  echo fromurl URL
  echo "\tto get config file from url"
  echo
  echo fromenv
  echo "\tto get config from the environment variables"
  echo
  echo fromenv help
  echo "\tlist the environment variables"
  exit 1
fi
