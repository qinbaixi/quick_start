@echo off

rem ===================================
rem compile setting
SET compile_concurrency=8
rem ===================================

rem ===================================
rem nodes to start
SET start_nodes=10
rem ===================================

rem ===================================
rem Erlang node setting
rem ===================================
rem
rem IP
SET IP=127.0.0.1
rem
rem Your nick name
SET AUTHOR=qbx
rem
rem Erlang node Cookie
SET COOKIE=qbx
rem ===================================

rem Config name
rem ===================================
SET CONFIG=./config/quick_start

rem ===================================
rem app start
rem ===================================
SET START_ENTRANCE=game
