@echo off


call config.cmd


for %%i in (%start_nodes%) do (
    echo starting node %%i
    start start_one.bat %%i

    timeout /t 1
)

