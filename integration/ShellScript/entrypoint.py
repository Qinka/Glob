#!/usr/bin/env python

import subprocess,sys,os
import json
import urllib,urllib2
import re
import string


## Functions

def evalEnv(str):
  rePa = r'\$\(\w*\)'
  subs = re.findall(rePa,str)
  spls = re.split(rePa,str)
  finstr = ""
  for i in range(len(subs)):
    finstr += spls[i]
    finstr += evalEnv(getEnvStr(subs[i]))
  finstr += spls[len(subs)]
  return finstr

def getEnvStr(envName):
  rt = os.getenv(envName[2:len(envName)-1])
  if rt is None:
      rt = ""
  return rt


def fromurl(url):
    print "From url", url
    if len(url) >0:
        f = urllib.urlopen(url)
        writeToEtc(f.read())
        checkPassword()
        print 'Going to launch'
        execGlob()
        return True
    else:
        print "You need one more args to launch with a download config"
        return False

def fromenv():
    print "From environment variables"
    etcObj ={ 'port' : getEnvE(3000,'GLOB_PORT')
            , 'title' : getEnv('Glob','GLOB_TITLE')
            , 'log-path' : getEnv('stdout','GLOB_LOG_PATH')
            , 'public-key' : getEnv('/etc/glob/pubkey','GLOB_PUB_KEY')
            , 'listen-type' : getEnv('*','GLOB_TYPE')
            , 'database' :
                { 'hostaddr' : getEnv('localhost:27017','GLOB_DB_ADDR')
                , 'database' : getEnv('local','GLOB_DB_NAME')
                , 'access-mode' : getEnv('master','GLOB_DB_AM')
                , 'username' : getEnv('root','GLOB_DB_USR')
                , 'password' : getEnv('admin','GLOB_DB_PSK')
                , 'pool-kept-time' : getEnvE(100,'GLOB_DB_PK')
                , 'pool-strp-max': getEnvE(10,'GLOB_DB_PSM')
                , 'pool-stripes': getEnvE(10,'GLOB_DB_PSM')
                }
            }
    cfg = json.dumps(etcObj)
    writeToEtc(cfg)
    print "Going to launch"
    checkPassword()
    execGlob()
    return True

def fromlocal(path):
    fromlocal(path,False)

def fromlocal(path,k):
    print 'From local'
    if k and path != '/etc/glob/config':
        writeToEtc(open(path).read())
        path = '/etc/glob/config'
    execGlob(path)
        

def getEnv(d,env,k=False):
    e = evalEnv('$('+env+')')
    if len(e) == 0:
      printD("Env "+env+" with default "+str(d))
      return d
    else:
      return e
        
def getEnvE(d,env):
    env = '$(' + env + ')'
    rt = evalEnv(env)
    if len(rt) == 0:
        return d
    else:
        return eval(rt)
                                            
def checkPassword():
    return True

def execGlob(path='/etc/glob/config'):
    printD(open(path).read())
    os.system('glob-launch -c '+path +' +RTS -N -RTS')

def printD(text):
    isDebug = os.getenv("GLOB_DEBUG")
    if not (isDebug is None):
        print text
    
def writeToEtc(content):
    if not os.path.exists("/etc/glob"):
        printD("Create directory /etc/glob")
        os.mkdir("/etc/glob")
    cfg = open("/etc/glob/config","w")
    cfg.write(content)
    cfg.close()

## run ......

if len(sys.argv) > 1 and sys.argv[1] == 'from':
    if len(sys.argv) >3 and sys.argv[2] == 'url':
        fromurl(sys.argv[3])
    elif  len(sys.argv) >2 and sys.argv[2] == 'env':
        fromenv()
    elif  len(sys.argv) >3 and sys.argv[2] == 'local':
        isCp = len(sys.argv) >4 and sys.argv[4] == 'copy'
        fromlocal(sys.argv[3],isCp)
    else:
        raise Exception('You need to point out where you load config')
elif  len(sys.argv) > 1 and sys.argv[1] == 'help':
    print 'This is help...'
else:
    raise Exception('Here a "from" or "help" is needed.')
