import unicodedata
from pyquery import PyQuery as pq

SUPER_H = unicodedata.lookup('MODIFIER LETTER SMALL H')

filename = '03_morphology.html'
html = open(filename).read()
d = pq(html)

for p in d('p.style36'):
    print(pq(p).html().replace('<sup>h</sup>', SUPER_H))
