import re
import unicodedata
from pyquery import PyQuery as pq
from phonology import consonants

# Should find 924 roots.

SUPER_H = unicodedata.lookup('MODIFIER LETTER SMALL H')
EM_DASH = unicodedata.lookup('EM DASH')

stem_letters = set()
for c in consonants:
    stem_letters.update(c)

consonant_pattern = ''.join(sorted(stem_letters))
root_pattern = r'-\s*([%s]+)\s*-' % consonant_pattern

definition_patterns = [
    root_pattern + r'\s*‘(.*?)’',
    #root_pattern + r'\s*(.*?)' + EM_DASH,
    #root_pattern + r'\s*(.*?)--',
]

root_re = re.compile(root_pattern, re.UNICODE|re.IGNORECASE)

def clean_html(html):
    html = html.replace('<strong>', '')
    html = html.replace('</strong>', '')
    html = html.replace('<sup>h</sup>', SUPER_H)
    html = html.replace('&nbsp;', ' ')
    html = re.sub('\s+', ' ', html)
    return html

def clean_definition(text):
    text = text.replace('or', 'OR')
    return text

def extract_roots(html):
    html = clean_html(html)

    text = pq(html).text()

    #with open('lexicon_cleaned', 'w') as f:
    #    f.write(text)

    roots = set()
    for match in root_re.finditer(text):
        roots.add(match.group(1))
    return(sorted(roots))

def extract_definitions(html):
    html = clean_html(html)
    d = pq(html)

    definitions = set()

    # Extract definitions from tables.
    pattern = definition_patterns[0]
    definition_re = re.compile(pattern, re.UNICODE|re.IGNORECASE)
    for table in d('table'):
        first_td = pq(table)('td:first')[0]
        text = pq(first_td).text()
        print(root_re.findall(text))
        m = definition_re.search(text)
        if m:
            definition = m.group(2)
            definition = clean_definition(definition)
            print(definition)
        else:
            pass
            print('-')
    return

    definitions = set()
    for pattern in definition_patterns:
        definition_re = re.compile(pattern, re.UNICODE|re.IGNORECASE)
        for match in definition_re.finditer(text):
            definitions.add((
                match.group(1), # root
                match.group(2), # definition
            ))
    defined_roots = set(r for (r, d) in definitions)

    undefined_roots = set(
        r for r in roots
        if r not in defined_roots
    )

    print('roots', len(roots))
    print('definitions', len(definitions))
    print('defined roots', len(defined_roots))
    print('undefined roots', len(undefined_roots))
    #for r in sorted(undefined_roots):
    #    print(r)
    #return

    #roots = sorted(roots)
    #definitions = sorted(definitions)

    #return roots
    return sorted(definitions)

if __name__ == '__main__':
    filename = 'lexicon.htm'
    html = open(filename).read()
    roots = extract_roots(html)
    print('found {} roots'.format(len(roots)))
    definitions = extract_definitions(html)
    #for root, definition in definitions:
        #print('{}: {}'.format(root, definition))
