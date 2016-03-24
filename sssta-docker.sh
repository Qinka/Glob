#!/bin/sh

docker run  -d --name qinka-glob -p 3000:3000 -e GLOB_PORT=3000 -e GLOB_CONTHD=20 -e STATIC_PATH=/ -e SITE_TITLE=TestSite -e GLOB_BACKEND_TOKEN="01 12 23 34 45 56 67 78 89 9a ab bc cd de ef f0" -e DB_PORT=5432 -e DB_ADDR=192.168.1.136 -e DB_NAME=postgres -e DB_PSK=johnjing -e DB_USR=qinka qinka/glob:postgresql-93c9a33b7cb9dfbaf91171d905c1561c6d20fa38-134
