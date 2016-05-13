#/bin/bash
create 80 $PORT_BEGIN $PORT_END $WORKER_CONNECTIONS \'$DOMAIN\' start.sh > /etc/nginx/nginx.conf
bash start.sh
