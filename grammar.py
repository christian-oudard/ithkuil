designations = ('FML', 'IFL')

configurations = (
    'UNI', 'DPX', 'DCT', 'AGG', 'SEG', 'CPN', 'COH', 'CST', 'MLT',
)
affiliations = ('CSL', 'ASO', 'VAR', 'COA')
perspectives = ('M', 'U', 'N', 'A')
extensions = ('DEL', 'PRX', 'ICP', 'TRM', 'DPL', 'GRA')
essences = ('NRM', 'RPV')

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

    ca_table = {}
    ca_table_reverse = {}
    for ca, key in zip(lines, keys):
        ca = ca.strip()
        ca_table[key] = ca
        ca_table_reverse[ca] = key
    return ca_table, ca_table_reverse

ca_table, ca_table_reverse = gen_ca_tables()
