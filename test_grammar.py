from nose.tools import assert_equal
from grammar import deconstruct_formative

cases = [
    (
        'eqal',
        [
            ('Tone', 'falling', 'PRC'),
            ('Vr', 'e', ('STA', 'P1', 'S2')),
            ('Cr', 'q', 'higher order animal life'),
            ('Vc', 'a', ('OBL',)),
            ('Ci+Vi', '', ('ASR', 'FAC')),
            ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
            ('Vf', '', ('EXS', 'NOFORMAT')),
            ('Cb', '', ('NOBIAS',)),
        ],
    ),
    (
        'pʰal',
        [
            ('Tone', 'falling', 'PRC'),
            ('Vr', '', ('STA', 'P1', 'S1')),
            ('Cr', 'pʰ', 'branched and / or leaved plant'),
            ('Vc', 'a', ('OBL',)),
            ('Ci+Vi', '', ('ASR', 'FAC')),
            ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
            ('Vf', '', ('EXS', 'NOFORMAT')),
            ('Cb', '', ('NOBIAS',)),
        ],
    ),
    (
        '¯uakal',
        [
            ('Tone', 'high', 'CPT'),
            ('Vr', 'ua', ('DYN', 'P3', 'S1')),
            ('Cr', 'k', 'path-oriented translative motion'),
            ('Vc', 'a', ('OBL',)),
            ('Ci+Vi', '', ('ASR', 'FAC')),
            ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
            ('Vf', '', ('EXS', 'NOFORMAT')),
            ('Cb', '', ('NOBIAS',)),
        ],
    ),
    (
        'Ilmašqi',
        [
            ('Tone', 'falling', 'PRC'),
            ('Vr', 'i', ('DYN', 'P1', 'S1')),
            ('Cr', 'lm', 'music'),
            ('Vc', 'a', ('OBL',)),
            ('Ci+Vi', '', ('ASR', 'FAC')),
            ('Ca', 'šq', ('NRM', 'DEL', 'M', 'COA', 'CST')),
            ('Vf', 'i', ('FNC', 'NOFORMAT')),
            ('Cb', '', ('NOBIAS',)),
        ],
    ),
    (
        'eglayës',
        [
            ('Tone', 'falling', 'PRC'),
            ('Vr', 'e', ('STA', 'P1', 'S2')),
            ('Cr', 'gl', 'state of health / illness / well-being'),
            ('Vc', 'a', ('OBL',)),
            ('Ci+Vi', 'yë', ('ASR', 'ASM')),
            ('Ca', 's', ('NRM', 'PRX', 'M', 'CSL', 'UNI')),
            ('Vf', '', ('EXS', 'NOFORMAT')),
            ('Cb', '', ('NOBIAS',)),
        ],
    ),
    (
        'isvala’kss',
        [
            ('Tone', 'falling', 'PRC'),
            ('Vr', 'i', ('DYN', 'P1', 'S1')),
            ('Cr', 'sv', 'fear / fright'),
            ('Vc', 'a', ('OBL',)),
            ('Ci+Vi', '', ('ASR', 'FAC')),
            ('Ca', 'l', ('NRM', 'DEL', 'M', 'CSL', 'UNI')),
            ('Vf', 'a', ('EXS', 'NOFORMAT')),
            ('Cb', '’kss', ('SKP+',)),
        ],
    ),
]

def test_deconstruct_formative():
    for word, target in cases:
        def test(word):
            assert_equal(
                deconstruct_formative(word),
                target,
            )
        yield test, word
