#!/bin/bash
cabal build
cabal run < $1 > out.j
java -jar jasmin.jar out.j

