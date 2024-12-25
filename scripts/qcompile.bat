chcp 65001

@ECHO off

:: 标题
TITLE qcompile

call config.cmd

CD ..

erlc -o ebin ./src/tools/qmake.erl

ECHO "Compiling game in [%compile_currency%] worker..."
erl -noshell -pa ebin -eval "case qmake:compile(%compile_currency%) of success -> halt(0); error -> halt(1) end."
SET compile_res=%ERRORLEVEL%
echo "erl halt return : [%compile_res%]"
ECHO "Compile finished !!!"
PAUSE