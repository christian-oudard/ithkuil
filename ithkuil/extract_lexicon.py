import re
import unicodedata
from pyquery import PyQuery as pq
from .phonology import consonants

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
    '^' + root_pattern + between + r"'(.*?)'",
    '^' + root_pattern + between + r'(.*?)' + EN_DASH,
    '^' + root_pattern + between + r'(.*?)' + EM_DASH,
    '^' + root_pattern + between + r'(.*?)--',
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

    roots = set()
    for match in root_re.finditer(text):
        roots.add(match.group(1))
    return(sorted(roots))

def extract_definitions(html):
    html = clean_html(html)
    d = pq(html)

    definitions = set()

    def add_definitions(text):
        for pattern in definition_patterns:
            definition_re = re.compile(pattern, re.UNICODE|re.IGNORECASE)
            m = definition_re.search(text)
            if m:
                root = m.group(1)
                definition = m.group(2)
                definition = clean_definition(definition)
                definitions.add((root, definition))
                break

    # Extract definitions from tables.
    for table in d('table'):
        first_td = pq(table)('td:first')[0]
        text = pq(first_td).text().strip()
        add_definitions(text)

    # Extract definitions from paragraphs.
    for paragraph in d('body p'):
        text = pq(paragraph).text().strip()
        roots = root_re.findall(text)
        if 'dry' in roots:
            print(text)
        if len(roots) > 0:
            add_definitions(text)

    return sorted(definitions)

if __name__ == '__main__':
    filename = 'lexicon.htm'
    html = open(filename).read()
    definitions = extract_definitions(html)

    check_correctness = False
    if check_correctness:
        roots = extract_roots(html)
        print('found {} roots'.format(len(roots)))
        print('found {} definitions'.format(len(definitions)))

        defined_roots = set(r for (r, d) in definitions)
        assert len(defined_roots) == len(definitions)
        undefined_roots = set(
            r for r in roots
            if r not in defined_roots
        )
        print('defined roots', len(defined_roots))
        print('undefined roots', len(undefined_roots))
        for r in sorted(undefined_roots):
            print(r)
        print(set(defined_roots) - set(roots))

    for root, definition in definitions:
        print('{}|{}'.format(root, definition))
