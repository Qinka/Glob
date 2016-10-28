#!/usr/bin/env python

import subprocess,sys,os
import json
import urllib,urllib2



## Functions

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
            , 'password-environment-variable' : getEnv('GLOB_PSK','GLOB_PSK_ENV')
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
    e = os.getenv(env)
    if not (e is None):
        ee = os.getenv(e[0:len(e)-3])
        if not (ee is None) and len(e)>1 and e[len(e)-2,len(e)-1] == "&&":
            printD("Env "+env+" with "+ee)
            if k:
                return eval(ee)
            else:
                return ee
        else:
            printD("Env "+env+" with "+e)
            if k:
                return eval(e)
            else:
                return e
    else:
        printD("Env "+env+" with default "+str(d))
        return d
        
def getEnvE(d,env):
    return getEnv(d,env,True)
                                            
def checkPassword():
    cfg = open("/etc/glob/config")
    c = cfg.read()
    cfg.close()
    j = json.loads(c)
    pskE = j['password-environment-variable']
    printD(pskE)
    env = os.getenv(pskE[1:len(pskE)-1])
    if env is None:
        print "Warning, no password set"
        return False
    return True

def execGlob(path='/etc/glob/config'):
    printD(open(path).read())
    os.execl('/usr/bin/glob-launch',' -c '+path +' +RTS -N -RTS')

def printD(text):
    isDebug = os.getenv("GLOB_DEBUG")
    if isDebug is None:
        print text
    
def writeToEtc(content):
    if not os.path.exists("/etc/glob"):
        printD("Create directory /etc/glob")
        os.mkdir("/etc/glob")
    cfg = open("/etc/glob/config","w")
    cfg.write(content)
    cfg.close()
        

    
## run .....

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
