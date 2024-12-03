chcp 65001

@ECHO off

:: 标题
TITLE qcompile

call config.cmd

CD ..

erlc -o ./ebin ./src/tools/qmake.erl

:start
SET choice=Y
ECHO "Compiling game ..."
erl -noshell -pa ebin -eval "case qmake:compile(%compile_concurrency%) of success -> halt(0); error -> halt(1) end."
ECHO "Compile finished !!!"
SET /P choice= 继续编译?(Y/N):
IF %choice%==N GOTO end
IF %choice%==n GOTO end

GOTO start
:end