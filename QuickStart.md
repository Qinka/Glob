Quick Start
===

This document is about how to start your site in a short time.
The following steps are the steps.

At first, there are two way to host(run) the glob.
They are running on docker and hosting on os.
The "Quick Start" is about the first one.

## Prepair

To host this site, we need somewhere to host our docker-containers.
We also two images. One is the PostgreSQL, and other is the Glob.
You can get PostgreSQL wherever you like, and you need to pull glob via:
```
docker pull qinka/glob:[TAGs(postgres-'commit'-'No.')]
```
If you do not want to host database on docker you can just launch a database
wherever you want. And if you want MongoDB to be the database rather than PostgreSQL,
you can choose the tags with "mongodb". And if you want use tls(https), you can
choose the tag with "tls".

Then you can launch the site with:
```
docker run  -d --name qinka-glob -p 3000:3000 -e GLOB_PORT=3000 -e GLOB_CONTHD=20 -e STATIC_PATH=/ -e SITE_TITLE=TestSite -e GLOB_BACKEND_TOKEN="01 12 23 34 45 56 67 78 89 9a ab bc cd de ef f0" -e DB_PORT=5432 -e DB_ADDR=192.168.1.136 -e DB_NAME=postgres -e DB_PSK=johnjing -e DB_USR=qinka qinka/glob:postgres-06c39e752851d145cbc7ed79379fbb77cf2b0b31-140
```

 | Enviroment varible | Description |
 | ----- | ----- |
 | GLOB_PORT | The port of site |
 | GLOB_CONTHD | The limitation of the number of the connection between site and database |
 | STATIC_PATH | Where site hold the files outside the database |
 | SITE_TITLE | The title of website |
 | GLOB_BACKEND_TOEKN | Token required when changing the context of site via background |
 | DB_PORT | The port of database |
 | DB_ADDR | The ip or host of database |
 | DB_NAME | The name of database |