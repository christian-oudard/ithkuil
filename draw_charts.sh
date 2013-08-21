#! /bin/bash

for file in charts/*.py; do
    base=${file/%.py/}
    python $file > $base.svg
done

for file in charts/*.svg; do
    base=${file/%.svg/}
    inkscape $base.svg --export-png=$base.png
    pngcrush $base.png $base.crushed.png
    convert $base.png $base.gif
done
