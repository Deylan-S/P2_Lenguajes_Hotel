#!/bin/sh
# Compila el proyecto. Ejecutar desde la carpeta que contiene "program":  sh program/build.sh
mkdir -p program/build
ghc -O1 -iprogram/src -iprogram/src/dataManagement -iprogram/src/Utils \
    program/app/Main.hs -outputdir program/build -o program/hotel
