Get A Start
===
In this part, you will know how to get a quick start of  Glob.

# Via Docker

The easiest way to get a quick start is using Docker.

## Pull Image

  For beginning, you need to pull the image of Glob and PostgreSQL.
  The image of Glob is unavaliable, so you need to build it by yourself.
  Or you can just use the one others build.But it will be avaliable soon.

## Host Image On Ship
 
  Glob has a launcher for docker. And you need set some environmental variables.
  
  You need set the following environmental variables:
  
  | variables' name | description |
  | ------ | --------- |
  | GLOB_PORT | The site's port, *KEEP UP WITH **Dockerfile***|
  | GLOB_BACKEND_TOKEN | |
  | DB_USR | |
  | DB_PORT | |
  | STATIC_PATH | |
  | DB_NAME | |
  | SITE_TITLE | |
  | CERTIFICATE_PATH | |
  | GLOB_CONTHD | |
  | DB_PSK | |
  | DB_ADDR | |
  | KEY_PATH| |

  Some of these need to keep up with your PostgreSQL image's settings.
  And the settings of PostgreSQL you need to search the Internet.
  
## Start Your
  
  And then you can launch it.
  
  tips:
    1. When using Docker, please make sure that the network is availbale between the containers of Glob and PostgreSQL
    2. Configure pg_hba.conf rightly. PostgreSQL some times only allow those who are in the list of pg_hba.conf to connecte database.
 
## Upload

  After it launched, you need upload you site. And [Qinka-Blog](https://github.com/Qinka/Qinka-Blog)
  is an example, and which is also my persion website.
  
  To upload, you need to clone Glob's source, and build a binary.It's source localed
  at Glob/src.bin/helper.identify.Use :
  ```
 $ ghc Main -i"../../src/"
  ```
  And this binary will help you in identify.At the same time, it is important
  that when some sources chanageed, you need to rebuild it, for the algorithm might be chanaged.
  
  And that application is just a tool to help you create tokens, and you might need cURL.

# Run in Local Environment

 It is easy to run Glob in any PC, when the OS is Linux.
But, until now, it is hard to run Glob on Windows. This is because some linker in Windows
will breake down when build Glob, even Yesod. And some time linker will also breake down,
on mac, but it  will be ok in ghci to run.

## Gi(e)t Source

Just run the following commands:
```
$ git clone https://github.com/Qinka/Glob.git
``` 

## Build

Make sure you installed GHC, and make sure that GHC's version is greater then 7.10 .

And if you need tls, when configuring via cabal, the flag -fwith-tls is need. 

Build Glob just need these:
```
$ cd Glob
$ cabal update
$ cabal configure
$ cabal install ... # Install dependance package
$ cabal install
```
The `cabal install` will build Glob and copy binary to `~/.cabal/bin` or `%AppData%\\.cabal\\bin`.
But if you just only what the binay without copy. The following is for this.
```
$ cd Glob
$ cabal update
$ cabal sandbox init
$ cabal install
```
The binary is avaliable at `.cabal_sandbox/bin` .
```
$ cd Glob
$ cabal update
$ cabal configure
$ cabal install ... # Install the dependance package
$ cabal build
```
The binary is avaliable at `dist/build`.

## Run

There are two way to launch Glob:
1. Using config file
1. Using launcher

Config file  example :
```json
{"foo":"bar"}
```
### Launcher

 When need launch, the flag -flaunch-docker and -flaunch-simple is needed.

Docker's launcher []

Simple launcher [TODO]

## Update

[SAME WITH DOCKER]