# This should be run from the root, i.e:
# python extract/convert_tables_yaml.py

from ithkuil.grammar import *
from ithkuil.phonology import convert_ascii_reverse


keys = list(itertools.product(*ca_order))
with open('data/ca.yaml', 'w') as f:
    for key in keys:
        affixes = ca_table[frozenset(key)]
        affixes = [convert_ascii_reverse(a) for a in affixes]

        affix_lines = [
            '  - {}'.format(a)
            for a in affixes
        ]
        line = '{}:\n{}\n'.format(
            '/'.join(key),
            '\n'.join(affix_lines),
        )
        f.write(line)
