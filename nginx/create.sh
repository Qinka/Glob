#!/bin/bash
if [ $# -eq 6 ];then
  let port="$1"
  let begp="$2"
  let endp="$3"
  let conn="$4"
  export domain="$5"
  export startsh=$6
  # /etc/nginx/nginx.conf
  echo "daemon off;"
  echo "  events"
  echo "  {"
  echo "    worker_connections" $conn ";"
  echo "  }"
  echo "  http"
  echo "  {"
  echo "    server"
  echo "    {"
  echo "      listen" $port ";"
  echo "      server_name" $domain "; "
  echo "      location /"
  echo "      {"
  echo "        proxy_pass http://glob;"
  echo "      }"
  echo "    }"
  echo "    upstream glob"
  echo "    {"
  echo "#!/bin/sh" > $startsh
  chmod a+x $startsh
  let i=$begp
  while [ $i -le $endp ]
  do
    echo "      server 127.0.0.1:"$i";"
    echo "export GLOB_PORT$i=$i"  >> $startsh
    echo "launch.docker --port=GLOB_PORT"$i" +RTS -N &" >> $startsh
    let i=i+1
  done
  echo "nginx" >> $startsh
  echo "    }"
  echo "  }"
else
  #参数为0
  echo "使用方法"
  echo "实际端口号 内部进程端口号起始 终止 工作链接个数 域名s"
fi
