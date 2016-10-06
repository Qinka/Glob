#!/bin/bash

###
### The entry point of Glob.
###

##
## FUNCTION
##


# from url
function fromurl #(url)
{
    echo From url >&2
    if [ $# -eq 3 ]; then
	mkEtcGlob
	curl $3 > /etc/glob/config
	checkPsk
	execGlob
    else
	echo You need one more arg to launch with a download config. >&2
	exit 1;
    fi
}
# from env
function fromenv
{
    echo From env >&2
    mkEtcGlob
    globPort=$(getEnv 3000 GLOB_PORT)
    globDbAddr=$(getEnv "localhost:27017" GLOB_PORT)
    globDbName=$(getEnv local GLOB_DB_NAME)
    globDbAm=$(getEnv master GLOB_DB_AM)
    globDbUsr=$(getEnv root GLOB_DB_USR)
    globDbPsk=$(getEnv admin GLOB_DB_PSK)
    globDbPs=$(getEnv 10 GLOB_DB_PS)
    globDbPk=$(getEnv 100 GLOB_DB_PK)
    globDbPsm=$(getEnv 10 GLOB_DB_PSM)
    globTitle=$(getEnv Glob GLOB_TITLE)
    globPskEV=$(getEnv GLOB_PSK GLOB_PSK_ENV_VAR)
    globLogP=$(getEnv stdout GLOB_LOG_PATH)
    globType=$(getEnv '*' GLOB_TYPE)
    echo "{ \"port\" : $globPort , \"database\" :{ \"hostaddr\" : \"$globDbAddr\" " \
       ", \"database\" : \"$globDbName\" , \"access-mode\": \"$globDbAm\" " \
       ", \"username\" : \"$globDbUsr\" , \"password\":\"$globDbPsk\" , \"pool-stripes\" : $globDbPs " \
       ", \"pool-kept-time\" : $globDbPk , \"pool-strp-max\" : $globDbPsm }" \
       ", \"title\" : \"$globTitle\" , \"password-environment-variable\" : \"$globPskEV\" " \
       ", \"log-path\" : \"$globLogP\" , \"listen-type\" : \"$globType\" }" > /etc/glob/config
    checkPsk
    execGlob
}
function fromlocal
{
    echo From local >&2
    if [ $# -eq 4 ]; then
	if [ "$4" != "/etc/glob/config" ]; then
	    mkEtcGlob
	    cat $4 > /etc/glob/config
	fi
	checkPsk
	execGlob
	exit 0
    else
	echo You need one more arg to launch with a local config.
	exit 1
    fi
}
# make directory /etc/glob
function mkEtcGlob
{
    if [ ! -d "/etc/glob" ]; then
	mkdir /etc/glob
    fi
}
# check password work or not
function checkPsk
{
    if [ -z "$(printenv $(cat /etc/glob/config | jq '."password-enviroment-variable"' -r))" ]; then
	echo -e "\a"  >&2
	echo "WARNING the environment variable where should have held password is empty, of null. You might be unabel to upload files." >&2
    fi
}
# to run Glob
function execGlob
{
    cat /etc/glob/config >&2
    glob-launch -c /etc/glob/config +RTS -N -RTS
}
# for get var
function getEnv
{
    echo "$@" >&2
    if [ $# -eq 2 ] && [ -z "$(printenv $2)" ] ; then
	echo Using the default of "$2"  "$1" >&2
	echo "$1"
	exit 0
    elif [ $# -eq 2 ]; then
	count=$(echo "$2" | awk '{print NF}')
	flag1=$(echo "$2" | awk '{print $1}')
	env=$(echo "$2" | awk '{print $2}') 
	if [ $count -gt 1 ] && [ "$flag1" = "env" ]; then
	    echo "$(printenv $env)"
	else
	    echo "$2"
	fi
	exit 0
    else
	echo the number of args is wrong. >&2
	exit 1
    fi
}

	


# Let it stop when there are an error
set -e

if [ "$1" = "from" ]; then
    # for from url
    if [ "$2" = "url" ]; then
	fromurl $@
    # for from env
    elif [ "$2" = "env" ]; then
	fromenv
    # for from local file
    elif [ "$2" = "local" ] && [ "$3" = "file" ]; then
	fromlocal $@
    else
	echo You need to point out where you load config. >&2
	exit 1;
    fi
elif [ "$1" = "help" ]; then
    #
    echo TODO >&2
else # Fail to do any thing.
    echo Here a '"from"' or '"help"' is needed. >&2
    exit 1
fi
