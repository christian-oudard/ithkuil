import itertools
import unicodedata

consonants = (
    'p', 'b', 'pʰ', 'p’', 'm',
    'w', 'f', 'v', 't', 'd', 'tʰ', 't’', 'ţ', 'dh', 'n',
    'c', 'ż', 'cʰ', 'c’', 's', 'z',
    'r',
    'č', 'j', 'čʰ', 'č’', 'š', 'ž',
    'ç', 'y',
    'k', 'g', 'kʰ', 'k’', 'x', 'ň',
    'q', 'qʰ', 'q’', 'xh', 'ř',
    '’',
    'h',
    'ļ', 'l',
)
typing_consonants = (
    'p', 'b', 'ph', "p'", 'm',
    'w',
    'f', 'v',
    't', 'd', 'th', "t'", 't,', 'dh', 'n',
    'c', 'dz', 'ch', "c'", 's', 'z',
    'r',
    'c^', 'j', 'c^h', "c^'", 's^', 'z^',
    'c,', 'y',
    'k', 'g', 'kh', "k'", 'x', 'n^',
    'q', 'qh', "q'", 'xh', 'r^',
    "'",
    'h',
    'l,', 'l',
)
vowels = (
    'î', 'ü', 'û',
    'i', 'u',
    'ê', 'ë', 'ô',
    'e', 'ö', 'o',
    'a', 'â',
)
typing_vowels = (
    'i^', 'u:', 'u^',
    'i', 'u',
    'e^', 'e:', 'o^',
    'e', 'o:', 'o',
    'a', 'a^',
)
tones = {
    'falling': '',
    'low': '_',
    'high': unicodedata.lookup('MACRON'),
    'rising': unicodedata.lookup('ACUTE ACCENT'),
    'fallingrising': unicodedata.lookup('CARON'),
    'risingfalling': '^',
}
typing_tones = {
    'falling': '',
    'low': '_',
    'high': '=',
    'rising': '/',
    'fallingrising': '~',
    'risingfalling': '^',
}

_conversion_table = {}
for t, c in itertools.chain(
    zip(typing_consonants, consonants),
    zip(typing_vowels, vowels),
):
    _conversion_table[t] = c
    _conversion_table[t.upper()] = c.upper()
_max_combo_length = max(
    len(t) for t in _conversion_table.keys()
)

def convert_typed(typed_text):
    """
    >>> convert_typed("Ikc,as")
    'Ikças'
    >>> convert_typed("Ikc,as")
    'Ikças'
    >>> convert_typed("A'tukc,as te^ oxnall")
    'A’tukças tê oxnall'
    >>> convert_typed("U^b eikkradwa smou'ola^xh.")
    'Ûb eikkradwa smou’olâxh.'

    #>>> convert_typed("=Sakc^'a to^ myicka zboack.")
    #'ˉSakč’a tô myicka zboack.'
    """
    result = []
    i = 0
    typed_length = len(typed_text)
    while i < typed_length:
        # Try greedily matching combinations.
        for length in range(_max_combo_length, 0, -1):
            combo = typed_text[i:i+length]
            converted = _conversion_table.get(combo)
            if converted:
                result.append(converted)
                break
        else:
            result.append(combo)
        i += length
    return ''.join(result)
