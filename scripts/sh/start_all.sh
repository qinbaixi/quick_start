#!/bin/sh
BUILD_ID=dontKillMe
source ./config.sh

function start_screen_node(){
    node_id=$1
    echo "screen -dmS ${node_id} -s ./start_one.sh"
    screen -dmS ${node_id} -s ./start_one.sh

    sleep 1
}

for node_id in ${start_nodes[*]}
do
    start_screen_node $node_id
done

# start_screen_node 100
# start_screen_node 300
# start_screen_node 10
# start_screen_node 11

screen -ls | grep ${AUTHOR}

exit 0