#!/usr/bin/env bash
set -euo pipefail

for prog in $*; do
    echo "Compiling file: $(basename $prog)"
    stack run -j 6 -- -v "$prog"
    # echo "Programmergebnis ist: "
    # ./code
done
