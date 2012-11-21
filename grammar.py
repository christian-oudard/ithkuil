import itertools
from phonology import convert_typed

functions = ('STA', 'DYN', 'MNF', 'DSC')
patterns = ('P1', 'P2', 'P3')
stems = ('S1', 'S2', 'S3')
cases = (
    'OBL', 'IND', 'ABS', 'ERG', 'EFF', 'AFF', 'DAT', 'INS', 'ACT', 'DER',
    'SIT', 'POS', 'PRP', 'GEN', 'ATT', 'PDC', 'ITP', 'OGN', 'PAR', 'CRS',
    'CPS', 'PRD', 'MED', 'APL', 'PUR', 'CSD', 'ESS', 'ASI', 'FUN', 'TFM',
    'REF', 'CLA', 'CNV', 'IDP', 'BEN', 'TSP', 'CMM', 'COM', 'CNJ', 'UTL',
    'ABE', 'CVS', 'COR', 'DEP', 'PVS', 'PTL', 'CON', 'EXC', 'AVR', 'CMP',
    'SML', 'ASS', 'CNR', 'ACS', 'DFF', 'PER', 'PRO', 'PCV', 'PCR', 'ELP',
    'ALP', 'INP', 'EPS', 'PLM', 'LIM', 'LOC', 'ORI', 'PSV', 'ALL', 'ABL',
    'NAV', 'VOC',
)
configurations = (
    'UNI', 'DPX', 'DCT', 'AGG', 'SEG', 'CPN', 'COH', 'CST', 'MLT',
)
affiliations = ('CSL', 'ASO', 'VAR', 'COA')
perspectives = ('M', 'U', 'N', 'A')
extensions = ('DEL', 'PRX', 'ICP', 'TRM', 'DPL', 'GRA')
essences = ('NRM', 'RPV')
designations = ('FML', 'IFL')

def lines_to_tables(lines, keys):
    # Ignore order of multi-part keys.
    keys = [frozenset(k) for k in keys]

    # Read each line, split multiple affixes, and create 
    # bidirectional dictionaries.
    table = {}
    table_reverse = {}
    for affixes, key in zip(lines, keys):
        affixes = affixes.split('|')
        table[key] = tuple(sorted(affixes))
        for affix in affixes:
            table_reverse[affix] = key
    return table, table_reverse

# Lexicon.
def gen_lexicon():
    lexicon_table = {}
    with open('lexicon.dat') as f:
        for line in f.readlines():
            root, definition = line.strip().split('|')
            lexicon_table[root] = definition
    return lexicon_table
lexicon_table = gen_lexicon()

# Vr affix.
vr_order = [functions, patterns, stems]
def gen_vr_tables():
    keys = list(itertools.product(*vr_order))

    with open('vr_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
vr_table, vr_table_reverse = gen_vr_tables()

# Vc affix.
vc_order = [cases]
def gen_vc_tables():
    keys = list(itertools.product(*vc_order))

    with open('vc_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    # Handle Vr substitution for cases 51 - 71.
    # For now, just always substitute "a".
    lines = [line.replace('V', 'a') for line in lines]

    return lines_to_tables(lines, keys)
vc_table, vc_table_reverse = gen_vc_tables()

# Ca affix.
ca_order = [essences, extensions, perspectives, affiliations, configurations]
def gen_ca_tables():
    keys = list(itertools.product(*ca_order))

    with open('ca_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
ca_table, ca_table_reverse = gen_ca_tables()

# Canonical key representations.
canonical_keys = {}
for ordering in [
    vr_order,
    vc_order,
    ca_order,
]:
    for key in itertools.product(*ordering):
        canonical_keys[frozenset(key)] = key


def lookup(query):
    """
    Lookup a bit of pronunciation or grammar. Accepts pronunciation fragments,
    IME text, grammar table keys, and string representations of table keys.

    Simple affix lookup.
    >>> lookup('l')
    [('NRM', 'DEL', 'M', 'CSL', 'UNI')]
    >>> lookup("ei'a")
    [('PCR',)]

    IME text works too.
    >>> lookup('üö')
    [('DSC', 'P3', 'S3')]
    >>> lookup('u:o:')
    [('DSC', 'P3', 'S3')]

    Lookup of grammatical keys, in tuple and text format.
    >>> lookup('PCR')
    ['ei’a']
    >>> lookup(('NRM', 'DEL', 'M', 'CSL', 'UNI'))
    ['l']
    >>> lookup('NRM/DEL/m/CSL/DPX')
    ['ll']
    >>> lookup('nrm/del/m/csl/dpx')
    ['ll']

    Order is irrelevant.
    >>> lookup('STA/P1/S1')
    ['', 'a']
    >>> lookup('P1/S1/STA')
    ['', 'a']
    """
    result = []

    if isinstance(query, (list, tuple, set, frozenset)):
         return lookup_key(query)

    # Try to interpret the query as slash-separated key values instead of tuple
    # key values.
    # Do this before adding phonology.
    pieces = query.split('/')
    result.extend(lookup_key(pieces))

    # Try to interpret the query as an affix.
    query = convert_typed(query)
    for rtable in [
        vr_table_reverse,
        vc_table_reverse,
        ca_table_reverse,
    ]:
        key = rtable.get(query)
        if key is not None:
            result.append(canonical_keys[key])

    return result

def lookup_key(key):
    result = []

    # Normalize key case.
    key = frozenset(d.upper() for d in key)

    # Look up key in tables.
    for table in [
        vr_table,
        vc_table,
        ca_table,
    ]:
        affixes = table.get(key)
        if affixes is not None:
            result.extend(affixes)

    return result

def lookup_lexicon(root):
    """
    >>> lookup_lexicon('ph')
    'branched and / or leaved plant'
    """
    root = convert_typed(root)
    root = root.upper()
    return lexicon_table.get(root)
