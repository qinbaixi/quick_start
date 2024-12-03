@echo off

call config.cmd

SET NODE_ID=%1

SET NODE_NAME=%AUTHOR%_%NODE_ID%@%IP%


cd ..

start werl +P 102400 -name %NODE_NAME% -setcookie %COOKIE% -hidden -config %CONFIG% -smp auto -pa ebin ebin_deps -s reloader -s %START_ENTRANCE% start -extra %NODE_ID%

exit