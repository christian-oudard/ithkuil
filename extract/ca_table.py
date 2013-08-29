import unicodedata
import re
from pyquery import PyQuery as pq

SUPER_H = unicodedata.lookup('MODIFIER LETTER SMALL H')

if __name__ == '__main__':
    filename = '03_morphology.html'
    html = open(filename).read()
    d = pq(html)

    for p in d('p.style36'):
        text = pq(p).html()
        text = re.sub('\s', '', text)
        text = text.replace('<sup>h</sup>', SUPER_H)
        affixes = text.split('/')
        print('|'.join(affixes))
