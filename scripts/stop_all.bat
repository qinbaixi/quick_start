@echo off

call config.cmd

for %%i in (%start_nodes%) do (
    echo stopping node %%i
    start stop_one.bat %%i
)