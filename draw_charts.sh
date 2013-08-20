#! /bin/bash

for file in charts/*.py; do
    base=${file/%.py/}
    python $file > $base.svg && \
    inkscape $base.svg --export-png=$base.png
    convert $base.png $base.gif
    pngcrush $base.png $base.crushed.png
    mv $base.crushed.png $base.png
done
