import re
import unicodedata
from pyquery import PyQuery as pq
from phonology import consonants

# Should find 924 roots.

SUPER_H = unicodedata.lookup('MODIFIER LETTER SMALL H')
EN_DASH = unicodedata.lookup('EM DASH')
EM_DASH = unicodedata.lookup('EM DASH')

stem_letters = set()
for c in consonants:
    stem_letters.update(c)

consonant_pattern = ''.join(sorted(stem_letters))
root_pattern = r'-\s*([%s]+)\s*-' % consonant_pattern
between = r'[\s:]*'

definition_patterns = [
    root_pattern + between + r"'(.*?)'",
    root_pattern + between + r'(.*?)' + EN_DASH,
    root_pattern + between + r'(.*?)' + EM_DASH,
    root_pattern + between + r'(.*?)--',
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
    text = text.lower().strip()
    text = text.replace('[', '(')
    text = text.replace(']', ')')
    text = re.sub(r'(\w)\/(\w)', r'\1 / \2', text)
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

    def add_definitions(text):
        print(text)
        for pattern in definition_patterns:
            definition_re = re.compile(pattern, re.UNICODE|re.IGNORECASE)
            m = definition_re.search(text)
            if m:
                root = m.group(1)
                definition = m.group(2)
                definition = clean_definition(definition)
                print()
                print(definition)
                definitions.add((root, definition))
                break
        else:
            print('-')

        print('*'*80)

    # Extract definitions from tables.
    #for table in d('table'):
        #first_td = pq(table)('td:first')[0]
        #text = pq(first_td).text()
        #add_definitions(text)

    # Extract definitions from paragraphs.
    for paragraph in d('body p'):
        text = pq(paragraph).text()
        roots = root_re.findall(text)
        if len(roots) > 0:
            print(roots)
            #print('*'*80)
            #print(text)
            add_definitions(text)

    return definitions


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
