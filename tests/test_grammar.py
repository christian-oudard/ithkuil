from nose.tools import assert_equal
from ithkuil.grammar import deconstruct_formative, lookup, lookup_lexicon


def test_deconstruct_formative():
    cases = [
        (
            'eqal',
            [
                ('Tone', 'falling', ('PRC',)),
                ('Vr', 'e', ('STA', 'P1', 'S2')),
                ('Cr', 'q', 'higher order animal life'),
                ('Vc', 'a', ('OBL',)),
                ('Ci+Vi', '', ('ASR', 'FAC')),
                ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
                ('Vf', '', ('EXS', 'NOF')),
                ('Cb', '', ('NOB',)),
            ],
        ),
        (
            'pʰal',
            [
                ('Tone', 'falling', ('PRC',)),
                ('Vr', '', ('STA', 'P1', 'S1')),
                ('Cr', 'pʰ', 'branched and / or leaved plant'),
                ('Vc', 'a', ('OBL',)),
                ('Ci+Vi', '', ('ASR', 'FAC')),
                ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
                ('Vf', '', ('EXS', 'NOF')),
                ('Cb', '', ('NOB',)),
            ],
        ),
        (
            '¯uakal',
            [
                ('Tone', 'high', ('CPT',)),
                ('Vr', 'ua', ('DYN', 'P3', 'S1')),
                ('Cr', 'k', 'path-oriented translative motion'),
                ('Vc', 'a', ('OBL',)),
                ('Ci+Vi', '', ('ASR', 'FAC')),
                ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
                ('Vf', '', ('EXS', 'NOF')),
                ('Cb', '', ('NOB',)),
            ],
        ),
        (
            'Ilmašqi',
            [
                ('Tone', 'falling', ('PRC',)),
                ('Vr', 'i', ('DYN', 'P1', 'S1')),
                ('Cr', 'lm', 'music'),
                ('Vc', 'a', ('OBL',)),
                ('Ci+Vi', '', ('ASR', 'FAC')),
                ('Ca', 'šq', ('NRM', 'DEL', 'M', 'COA', 'CST')),
                ('Vf', 'i', ('FNC', 'NOF')),
                ('Cb', '', ('NOB',)),
            ],
        ),
        (
            'eglayës',
            [
                ('Tone', 'falling', ('PRC',)),
                ('Vr', 'e', ('STA', 'P1', 'S2')),
                ('Cr', 'gl', 'state of health / illness / well-being'),
                ('Vc', 'a', ('OBL',)),
                ('Ci+Vi', 'yë', ('ASR', 'ASM')),
                ('Ca', 's', ('NRM', 'PRX', 'M', 'CSL', 'UNI')),
                ('Vf', '', ('EXS', 'NOF')),
                ('Cb', '', ('NOB',)),
            ],
        ),
        (
            'isvala’kss',
            [
                ('Tone', 'falling', ('PRC',)),
                ('Vr', 'i', ('DYN', 'P1', 'S1')),
                ('Cr', 'sv', 'fear / fright'),
                ('Vc', 'a', ('OBL',)),
                ('Ci+Vi', '', ('ASR', 'FAC')),
                ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
                ('Vf', 'a', ('EXS', 'NOF')),
                ('Cb', '’kss', ('SKP+',)),
            ],
        ),
    ]
    for word, target in cases:
        def test(word):
            assert_equal(
                deconstruct_formative(word),
                target,
            )
        yield test, word


def test_lookup():
    cases = [
        # Simple affix lookup.
        ('l', [('NRM', 'DEL', 'M', 'CSL', 'UNI')]),
        ("ei'a", [('PCR',)]),
        # IME text works too.
        ('üö', [('DSC', 'P3', 'S3')]),
        ('u:o:', [('DSC', 'P3', 'S3')]),
        # Lookup of grammatical keys, in tuple and text format.
        ('PCR', ['ei’a']),
        (('NRM', 'DEL', 'M', 'CSL', 'UNI'), ['l']),
        ('NRM/DEL/m/CSL/DPX', ['ll']),
        ('nrm/del/m/csl/dpx', ['ll']),
        # Order is irrelevant.
        ('STA/P1/S1', ['', 'a']),
        ('P1/S1/STA', ['', 'a']),
    ]
    for word, target in cases:
        def test(word):
            assert_equal(
                lookup(word),
                target,
            )
        yield test, word

def test_lookup_lexicon():
    cases = [
        ('ph', 'branched and / or leaved plant'),
    ]
    for stem, target in cases:
        def test(word):
            assert_equal(
                lookup_lexicon(stem),
                target,
            )
        yield test, stem
