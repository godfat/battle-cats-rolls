#!/bin/sh

root=$(realpath $(dirname $0)/..)
ghc -odir $root/build -hidir $root/build -O2 -threaded -with-rtsopts=-N -i$root $root/Main.hs -o $root/Seeker
