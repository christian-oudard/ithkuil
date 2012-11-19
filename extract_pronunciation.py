# coding: utf8

import sys
import os
import re
import unicodedata
from collections import defaultdict
from pyquery import PyQuery as pq

SUPER_H = unicodedata.lookup('MODIFIER LETTER SMALL H')

def unwrap(t):
    contents = pq(t).html()
    if contents is None:
        pq(t).remove()
    else:
        pq(t).replaceWith(contents)

def extract(d):
    # Remove redundant strong tags.
    for t in reversed(d('strong strong')):
        unwrap(t)

    # Replace super-h with the equivalent unicode.
    for t in d('sup'):
        if pq(t).html() == 'h':
            pq(t).replaceWith(SUPER_H)

    for e in d('strong'):
        for piece in pq(e).text().split('/'):
            yield piece, e

alphabet_normal = 'abcdefghijklmnopqrstuvwxyz'
alphabet_ithkuil_special = 'âçêëîôöûüčļňřšţżž'

def clean(text):
    text = text.strip()
    if len(re.sub('\W', '', text)) <= 3:
        return ''
    text = text.replace('\xa0', ' ') # Replace non-breaking spaces.
    text = re.sub('[ ]+', ' ', text)
    if '\n' in text:
        return ''
    if re.search('[0-9]', text):
        return ''
    if re.search('(^| )-', text):
        return ''
    if re.search('-($| )', text):
        return ''
    if not any(c in text for c in alphabet_ithkuil_special):
        return ''
    return text

if __name__ == '__main__':
    html_files = [
        f for f in os.listdir(os.getcwd())
        if f.endswith('.html')
    ]
    cleaned_to_original = defaultdict(list)
    for filename in html_files:
        # The filename keyword doesn't work for some reason?
        #d = pq(filename=filename)

        html = open(filename).read()
        d = pq(html)
        for text, tag in extract(d):
            cleaned = clean(text)
            if cleaned:
                cleaned_to_original[cleaned].append(str(pq(tag)))

    for cleaned in sorted(cleaned_to_original.keys()):
        originals = cleaned_to_original[cleaned]
        print(cleaned)
        #if len(originals) > 2 or originals[0] != cleaned:
            #print(originals)
