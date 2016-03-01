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
  
  After it launched, you need upload you site.