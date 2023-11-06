#!/bin/bash

cat ../testing | dune exec -- ./bin/main.exe --stdio --clientProcessId 5
