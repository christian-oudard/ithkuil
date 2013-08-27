import re
import unicodedata

from .util import choices_pattern


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
    # Transliteration only.
    'æ',
    'ø',
    'ɨ',
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
    # Transliteration only.
    'ae',
    'o%',
    'i+',
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


def make_conversion_table():
    table = {}
    for t, c in zip(typing_consonants + typing_vowels, consonants + vowels):
        table[t] = c
        table[t.upper()] = c.upper()
    for tone in tone_order:
        t = typing_tones[tone]
        c = tones[tone]
        table[t] = c
    return table
typing_conversion_table = make_conversion_table()


def _build_letter_regex():
    letters = list(typing_consonants + typing_vowels)
    for tone in tone_order:
        t = typing_tones[tone]
        if t != '':
            letters.append(t)
    pattern = choices_pattern(letters, split=True)
    return re.compile(pattern, re.IGNORECASE)
letter_regex = _build_letter_regex()


def _build_typing_vowel_regex():
    letters = list(typing_vowels)
    pattern = choices_pattern(letters, split=True)
    return re.compile(pattern, re.IGNORECASE)
typing_vowel_regex = _build_typing_vowel_regex()


def _build_typing_consonant_regex():
    letters = list(typing_consonants)
    pattern = choices_pattern(letters, split=True)
    return re.compile(pattern, re.IGNORECASE)
typing_consonant_regex = _build_typing_consonant_regex()


def split_ascii(ascii_text):
    """
    >>> split_ascii("Ikc,as")
    ['I', 'k', 'c,', 'a', 's']
    >>> split_ascii("=Sakc^'a")
    ['=', 'S', 'a', 'k', "c^'", 'a']

    Also works for transliterations.
    >>> split_ascii('kaet')
    ['k', 'ae', 't']
    >>> split_ascii('no%t')
    ['n', 'o%', 't']
    """
    return [
        s for s in letter_regex.split(ascii_text)
        if s != ''
    ]


def convert_ascii(ascii_text):
    """
    >>> convert_ascii("Ikc,as")
    'Ikças'
    >>> convert_ascii("A'tukc,as te^ oxnall")
    'A’tukças tê oxnall'
    >>> convert_ascii("U^b eikkradwa smou'ola^xh.")
    'Ûb eikkradwa smou’olâxh.'
    >>> convert_ascii("=Sakc^'a to^ myicka zboack.")
    '¯Sakč’a tô myicka zboack.'
    >>> convert_ascii("o\spa^tlo:k")
    'òspâtlök'
    >>> convert_ascii("Aigwapskh eks^u/ll,")
    'Aigwapskʰ ekšúlļ'
    >>> convert_ascii("a^pca^/l")
    'âpcââl'
    >>> convert_ascii("a^pca^a^l")
    'âpcââl'
    >>> convert_ascii('kaet')
    'kæt'
    >>> convert_ascii('no%t')
    'nøt'
    >>> convert_ascii('dzi+')
    'żɨ'
    """
    return ''.join(
        typing_conversion_table.get(s, s)
        for s in split_ascii(ascii_text)
    )


def convert_ascii_to_html(ascii_text):
    unicode_text = convert_ascii(ascii_text)
    return unicode_text.replace('ʰ', (
        '<tspan baseline-shift="33.3%" font-size="58.3%" '
        'font-weight="bold">h</tspan>'
    ))
