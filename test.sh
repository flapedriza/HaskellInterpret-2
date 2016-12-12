#!/bin/bash
if [ $# -ne 1 ]; then
echo "usage: $0 nom_fitxer_programa"
exit
fi

if [ ! -e $1 ]; then
echo "No existeix el fitxer $1"
exit
fi

if [ ! -e reader ]; then
antlr -gt reader.g
dlg -ci parser.dlg scan.c
g++ -o reader reader.c scan.c err.c -I/usr/include/pccts -Wno-write-strings
rm *.c *.h *.dlg
fi

./reader < $1 > programhs.txt

runhaskell program.hs