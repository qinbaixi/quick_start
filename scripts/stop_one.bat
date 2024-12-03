@echo off


call config.cmd

SET NODE_ID=%1

SET NODE_NAME=%AUTHOR%_%NODE_ID%@%IP%

SET STOP_NAME=stop_%NODE_NAME%

cd ..

echo "erl -detached -name %STOP_NAME% -setcookie %COOKIE% -hidden -eval "rpc:call('%NODE_NAME%', %START_ENTRANCE%, stop, [])" -s init stop"

erl -detached -name %STOP_NAME% -setcookie %COOKIE% -hidden -eval "rpc:call('%NODE_NAME%', %START_ENTRANCE%, stop, [])" -s init stop