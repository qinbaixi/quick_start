#!/bin/sh

source ./config.sh

NODE_ID=$1
NODE_NAME=${AUTHOR}_${NODE_ID}@${IP}

cd ..

erl +MIscs 2048 +P 1048576 -kernel inet_dist_listen_min 40001 -kernel inet_dist_listen_max 40500 -smp auto -name ${NODE_NAME} -setcookie ${COOKIE} -hidden -config ${CONFIG} -smp auto -pa ebin -pa ebin_deps -s ${START_ENTRANCE} start -extra ${NODE_ID}
