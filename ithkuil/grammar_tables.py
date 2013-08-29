import itertools
import yaml

"""
Formative structure:
(((Cv+)Vl+)Cg/Cs+)Vr+(Cx/Cv+Vp/Vl+)Cr+Vc(+Ci+Vi)+Ca(+Vx+Cx)(+Vf(+Cb))[+Tone][+Stress]
Cv: Phase + Sanction (+ Illocution)
Vl: Valence
Cg/Cs: Validation OR Aspect (+ Mood)
Vr: Pattern + Stem + Function
Cx/Cv: Incorporated Root OR Phase + Sanction (+ Illocution)
Vp/Vl: Pattern + Stem + Designation of Incorporated Root OR Valence
Cr: Root
Vc: Case
Ci+Vi: Illocution + Mood
Ca: Essence + Extension + Perspective + Configuration + Affiliation
Vx+Cx: Derivational Suffix(es)
Vf: Context + Format
Cb: Bias
Tone: Version
Stress: Designation + Relation
"""

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
    'CMP1A', 'CMP2A', 'CMP3A', 'CMP4A', 'CMP5A', 'CMP6A', 'CMP7A', 'CMP8A',
    'CMP1B', 'CMP2B', 'CMP3B', 'CMP4B', 'CMP5B', 'CMP6B', 'CMP7B', 'CMP8B',
    'CMP1C', 'CMP2C', 'CMP3C', 'CMP4C', 'CMP5C', 'CMP6C', 'CMP7C', 'CMP8C',
)
illocutions = ('ASR', 'DIR', 'IRG', 'ADM', 'HOR', 'DEC')
moods = ('FAC', 'SUB', 'ASM', 'SPC', 'COU', 'HYP', 'IPL', 'ASC')
configurations = (
    'UNI', 'DPX', 'DCT', 'AGG', 'SEG', 'CPN', 'COH', 'CST', 'MLT',
)
affiliations = ('CSL', 'ASO', 'VAR', 'COA')
perspectives = ('M', 'U', 'N', 'A')
extensions = ('DEL', 'PRX', 'ICP', 'TRM', 'DPL', 'GRA')
essences = ('NRM', 'RPV')
contexts = ('EXS', 'FNC', 'RPS', 'AMG')
formats = (
    'NOF', 'SCH', 'ISR', 'ATH', 'RSL',
    'SBQ', 'CCM', 'OBJ', 'PRT', 'AFI',
)
biases = (
    'NOB',
    'ASU', 'ASU+', 'HPB', 'HPB+', 'COI', 'COI+', 'ACP', 'ACP+', 'RAC', 'RAC+',
    'STU', 'STU+', 'CTV', 'CTV+', 'DPV', 'DPV+', 'RVL', 'RVL+', 'GRT', 'GRT+',
    'SOL', 'SOL+', 'SEL', 'SEL+', 'IRO', 'IRO+', 'EXA', 'EXA+', 'LTL', 'LTL+',
    'CRR', 'CRR+', 'EUP', 'EUP+', 'SKP', 'SKP+', 'CYN', 'CYN+', 'CTP', 'CTP+',
    'DSM', 'DSM+', 'IDG', 'IDG+', 'SGS', 'SGS+', 'PPV', 'PPV+',
)
versions = ('PRC', 'CPT', 'INE', 'INC', 'PST', 'EFC')
designations = ('FML', 'IFL')
relations = ('UNFRAMED', 'FRAMED')

categories = (
    functions,
    patterns,
    stems,
    cases,
    illocutions,
    moods,
    configurations,
    affiliations,
    perspectives,
    extensions,
    essences,
    contexts,
    formats,
    biases,
    versions,
    designations,
    relations,
)


# Orderings.

vr_order = [functions, patterns, stems]
vc_order = [cases]
civi_order = [illocutions, moods]
ca_order = [essences, extensions, perspectives, affiliations, configurations]
vf_order = [contexts, formats]
cb_order = [biases]


# Canonical key representations.

canonical_keys = {}
for ordering in [
    vr_order,
    vc_order,
    civi_order,
    ca_order,
    vf_order,
    cb_order,
]:
    for key in itertools.product(*ordering):
        canonical_keys[frozenset(key)] = key


def data_to_tables(data, keys):
    table = {}
    table_reverse = {}
    for key in keys:
        affixes = data['/'.join(key)]
        key = frozenset(key)  # Ignore order of multipart keys.
        table[key] = tuple(affixes)
        for affix in affixes:
            table_reverse[affix] = key
    return table, table_reverse


# Ca affix.
def make_ca_tables():
    keys = list(itertools.product(*ca_order))
    with open('data/ca.yaml') as f:
        data = yaml.load(f)
    return data_to_tables(data, keys)
    assert len(data) == len(keys)
ca_table, ca_table_reverse = make_ca_tables()
