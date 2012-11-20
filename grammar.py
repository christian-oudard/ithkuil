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

def gen_vr_tables():
    keys = [
        (fu, pa, st)
        for fu in functions
        for pa in patterns
        for st in stems
    ]

    with open('vr_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
vr_table, vr_table_reverse = gen_vr_tables()

def gen_vc_tables():
    keys = [
        (ca,)
        for ca in cases
    ]

    with open('vc_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    # Handle Vr substitution for cases 51 - 71.
    # For now, just always substitute "a".
    lines = [line.replace('V', 'a') for line in lines]

    return lines_to_tables(lines, keys)
vc_table, vc_table_reverse = gen_vc_tables()

def gen_ca_tables():
    keys = [
        (es, ex, pe, af, co)
        for es in essences
        for ex in extensions
        for pe in perspectives
        for af in affiliations
        for co in configurations
    ]

    with open('ca_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
ca_table, ca_table_reverse = gen_ca_tables()


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
    """
    result = []

    if isinstance(query, (list, tuple)):
         return lookup_key(query)

    # If we have a string, first try to interpret it as IME text.
    from phonology import convert_typed
    query = convert_typed(query)

    # Try to interpret the query as an affix.
    for rtable in [
        vr_table_reverse,
        vc_table_reverse,
        ca_table_reverse,
    ]:
        key = rtable.get(query)
        if key is not None:
            result.append(key)

    # Try to interpret the query as slash-separated key values instead of tuple
    # key values.
    pieces = query.split('/')
    result.extend(lookup_key(pieces))

    return result

def lookup_key(key):
    result = []

    # Normalize key case.
    key = tuple(d.upper() for d in key)

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
