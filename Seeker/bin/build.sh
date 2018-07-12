#!/bin/sh

ghc -odir build -hidir build -Odph -threaded -with-rtsopts=-N Main.hs -o Seeker
