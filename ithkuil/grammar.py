import itertools
import re
from .phonology import (
    convert_ascii,
    unicode_consonants,
    unicode_tones,
    unicode_tone_names,
)
from .util import choices_pattern

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


# Tables.

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


# Cr lexical root.
def make_lexicon():
    lexicon_table = {}
    with open('data/lexicon.dat') as f:
        for line in f.readlines():
            root, definition = line.strip().split('|')
            lexicon_table[root] = definition
    return lexicon_table
lexicon_table = make_lexicon()


# Vr affix.
def make_vr_tables():
    keys = list(itertools.product(*vr_order))

    with open('data/vr_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
vr_table, vr_table_reverse = make_vr_tables()


# Vc affix.
def make_vc_tables():
    keys = list(itertools.product(*vc_order))

    with open('data/vc_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    # TODO: Handle Vr substitution for cases 51 - 71.
    # For now, just always substitute "a".
    lines = [line.replace('V', 'a') for line in lines]

    return lines_to_tables(lines, keys)
vc_table, vc_table_reverse = make_vc_tables()


# Ci+Vi affixes.
def make_civi_tables():
    keys = list(itertools.product(*civi_order))

    # The last 7 combinations do not occur.
    keys = keys[:-7]

    with open('data/civi_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
civi_table, civi_table_reverse = make_civi_tables()


# Ca affix.
def make_ca_tables():
    keys = list(itertools.product(*ca_order))

    with open('data/ca_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
ca_table, ca_table_reverse = make_ca_tables()


# Vf affix.

def make_vf_tables():
    keys = list(itertools.product(*vf_order))

    with open('data/vf_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)

vf_table, vf_table_reverse = make_vf_tables()


# Cb affix.
def make_cb_tables():
    keys = list(itertools.product(*cb_order))

    with open('data/cb_table.dat') as f:
        lines = f.read().splitlines()
    assert len(lines) == len(keys)
    assert len(set(lines)) == len(lines)

    return lines_to_tables(lines, keys)
cb_table, cb_table_reverse = make_cb_tables()


# Tone.
version_table = {
    ('PRC',): unicode_tones['falling'],
    ('CPT',): unicode_tones['high'],
    ('INE',): unicode_tones['rising'],
    ('INC',): unicode_tones['low'],
    ('PST',): unicode_tones['fallingrising'],
    ('EFC',): unicode_tones['risingfalling'],
}
version_table_reverse = {tone: version for (version, tone) in version_table.items()}


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
    query = convert_ascii(query)
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
    root = convert_ascii(root)
    root = root.upper()
    return lexicon_table.get(root)


def deconstruct_formative(word):
    """
    Deconstruct a formative into its root and affixes, and lookup the meaning of each.

    Currently only handles the simplified structure:
    Vr+Cr+Vc(+Ci+Vi)+Ca(+Vf(+Cb))[+Tone]
    """
    word = convert_ascii(word).lower()
    parsed_word = parse_formative(word)
    if parsed_word is None:
        return None  # Could not understand word structure.
    tone, vr, cr, vc, civi, ca, vf, cb = parsed_word
    result = []

    version = version_table_reverse[tone]
    result.append(('Tone', unicode_tone_names[tone], version))

    vr_key = vr_table_reverse[vr]
    result.append(('Vr', vr, canonical_keys[vr_key]))

    meaning = lexicon_table[cr.upper()]
    result.append(('Cr', cr, meaning))

    vc_key = vc_table_reverse[vc]
    result.append(('Vc', vc, canonical_keys[vc_key]))

    civi_key = civi_table_reverse[civi]
    result.append(('Ci+Vi', civi, canonical_keys[civi_key]))

    ca_key = ca_table_reverse[ca]
    result.append(('Ca', ca, canonical_keys[ca_key]))

    vf_key = vf_table_reverse[vf]
    result.append(('Vf', vf, canonical_keys[vf_key]))

    cb_key = cb_table_reverse[cb]
    result.append(('Cb', cb, canonical_keys[cb_key]))

    return result


def _build_word_regex():
    word_pattern = r'''
        ^
        ({tone}) # Tone
        ({vr}) # Vr
        ({c}) # Cr
        ({vc}) # Vc
        ({civi})? # Ci + Vi
        ({c}) # Ca
        ({vf})? # Vf
        ({cb})? # Cb
        $
    '''.format(
        c=choices_pattern(unicode_consonants) + '+',
        vr=choices_pattern(vr_table_reverse.keys()),
        vc=choices_pattern(vc_table_reverse.keys()),
        civi=choices_pattern(civi_table_reverse.keys()),
        vf=choices_pattern(vf_table_reverse.keys()),
        cb=choices_pattern(cb_table_reverse.keys()),
        tone=choices_pattern(unicode_tones.values()),
    )
    return re.compile(word_pattern, re.UNICODE | re.IGNORECASE | re.VERBOSE)
word_regex = _build_word_regex()


def parse_formative(word):
    m = word_regex.match(word)
    if m:
        return tuple(m.groups())
