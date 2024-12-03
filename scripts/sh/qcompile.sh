#!/bin/sh

source ./config.sh

cd ..

erlc -o ebin ./src/tools/qmake.erl
erlc -I include -o ebin ./src/tools/lager/lager_rotator_behaviour.erl

echo "Start Compiling..."
echo
echo

erl -pa ./ebin -eval "case qmake:compile(${compile_concurrency}, []) of ok -> halt(0); _ -> halt(1) end." -noshell -s init stop

compile_res=$?

echo
echo

#exit 21

if [ $compile_res -eq 0 ]; then
        echo "Compile Success!"
        exit 0
fi

echo "Compile Failed!"
exit 1