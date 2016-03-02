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