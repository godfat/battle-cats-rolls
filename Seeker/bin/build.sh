#!/bin/sh

ghc -odir build -hidir build -O2 -threaded -with-rtsopts=-N Main.hs -o Seeker
