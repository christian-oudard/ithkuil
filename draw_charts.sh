#! /bin/bash

for file in charts/*.py; do
    base=${file/%.py/}
    python $file > ${base}.svg && \
    inkscape ${base}.svg --export-png=${base}.png
done
