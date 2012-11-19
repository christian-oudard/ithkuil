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
