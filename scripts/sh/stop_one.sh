#!/bin/sh

source ./config.sh

function stop_node(){
        node_id=$1

        echo "stopping node ${node_id} ..."

        node_name=${AUTHOR}_${NODE_ID}@${IP}
        stop_name=stop_${node_name}

        echo "erl -detached -name ${stop_name} -setcookie ${COOKIE} -hidden -eval \"rpc:call('${node_name}', ${START_ENTRANCE}, stop, [])\" -s init stop"

        erl -detached -name ${stop_name} -setcookie ${COOKIE} -hidden -eval "rpc:call('${node_name}', ${START_ENTRANCE}, stop, [])" -s init stop

        sleep 3
}

len=${#start_nodes[@]}
let len--
#for ((i=$len - 1;i>=0;i--))
for i in $(seq $len -1 0)
do
        stop_node ${start_nodes[i]}
done
#for node_id in ${start_nodes[*]}
#do
#       stop_node $node_id
#done

# screen -ls