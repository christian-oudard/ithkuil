import itertools
import unicodedata

# Letters.
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
    # Unmarked stress.
    'a', 'â',
    'e', 'ê', 'ë',
    'i', 'î',
    'o', 'ô', 'ö',
    'u', 'û', 'ü',
    # Stressed vowels.
    'á', 'ââ',
    'é', 'êê', 'ëë',
    'í', 'îî',
    'ó', 'ôô', 'öö',
    'ú', 'ûû', 'üü',
    # Unstressed vowels.
    'à',
    'è',
    'ì',
    'ò',
    'ù',
)
typing_vowels = (
    # Unmarked stress.
    'a', 'a^',
    'e', 'e^', 'e:',
    'i', 'i^',
    'o', 'o^', 'o:',
    'u', 'u^', 'u:',
    # Stressed vowels.
    'a/', 'a^/',
    'e/', 'e^/', 'e:/',
    'i/', 'i^/',
    'o/', 'o^/', 'o:/',
    'u/', 'u^/', 'u:/',
    # Unstressed vowels.
    'a\\',
    'e\\',
    'i\\',
    'o\\',
    'u\\',
)

_letter_conversion_table = {}
for t, c in itertools.chain(
    zip(typing_consonants, consonants),
    zip(typing_vowels, vowels),
):
    _letter_conversion_table[t] = c
    _letter_conversion_table[t.upper()] = c.upper()
_max_combo_length = max(

    len(t) for t in _letter_conversion_table.keys()
)

# Tones.
tone_order = [
    'falling',
    'high',
    'rising',
    'low',
    'fallingrising',
    'risingfalling',
]
tones = {
    'falling': '',
    'high': unicodedata.lookup('MACRON'),
    'rising': unicodedata.lookup('ACUTE ACCENT'),
    'low': '_',
    'fallingrising': unicodedata.lookup('CARON'),
    'risingfalling': '^',
}
tone_names = {c: name for (name, c) in tones.items()}
typing_tones = {
    'falling': '',
    'high': '=',
    'rising': '/',
    'low': '_',
    'fallingrising': '~',
    'risingfalling': '^',
}
assert (
    sorted(tone_order) ==
    sorted(tones.keys()) ==
    sorted(typing_tones.keys())
)
_tone_conversion_table = {}
for tone in tone_order:
    t = typing_tones[tone]
    c = tones[tone]
    _tone_conversion_table[t] = c

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
    >>> convert_typed("=Sakc^'a to^ myicka zboack.")
    '¯Sakč’a tô myicka zboack.'
    >>> convert_typed("o\spa^tlo:k")
    'òspâtlök'
    >>> convert_typed("Aigwapskh eks^u/ll,")
    'Aigwapskʰ ekšúlļ'
    >>> convert_typed("a^pca^/l")
    'âpcââl'
    >>> convert_typed("a^pca^a^l")
    'âpcââl'
    """
    result = []
    i = 0
    typed_length = len(typed_text)
    while i < typed_length:
        # Try greedily matching combinations.
        for length in range(_max_combo_length, 0, -1):
            combo = typed_text[i:i+length]
            converted = _letter_conversion_table.get(combo)
            if converted:
                result.append(converted)
                break
        else:
            # Possibly add tone markers at the beginning of the word.
            combo = typed_text[i]
            converted = _tone_conversion_table.get(combo)
            if converted and (i == 0 or typed_text[i-1].isspace()):
                result.append(converted)
            else:
                result.append(combo)
        i += length
    return ''.join(result)
