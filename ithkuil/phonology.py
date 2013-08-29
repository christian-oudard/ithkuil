import re
import unicodedata

from .util import choices_pattern


# Letters.
unicode_consonants = (
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
ascii_consonants = (
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
unicode_vowels = (
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
ascii_vowels = (
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
unicode_tones = {
    'falling': '',
    'high': unicodedata.lookup('MACRON'),
    'rising': unicodedata.lookup('ACUTE ACCENT'),
    'low': '_',
    'fallingrising': unicodedata.lookup('CARON'),
    'risingfalling': '^',
}
ascii_tones = {
    'falling': '',
    'high': '=',
    'rising': '/',
    'low': '_',
    'fallingrising': '~',
    'risingfalling': '^',
}
unicode_tone_names = {u: name for (name, u) in unicode_tones.items()}
ascii_tone_names = {a: name for (name, a) in ascii_tones.items()}
assert (
    set(tone_order) ==
    set(unicode_tones.keys()) ==
    set(ascii_tones.keys())
)


def make_conversion_tables():
    table = {}
    reverse_table = {}
    for a, u in zip(
        ascii_consonants + ascii_vowels,
        unicode_consonants + unicode_vowels
    ):
        table[a] = u
        table[a.upper()] = u.upper()
        reverse_table[u] = a
        reverse_table[u.upper()] = a.upper()
    for tone in tone_order:
        a = ascii_tones[tone]
        u = unicode_tones[tone]
        table[a] = u
        reverse_table[u] = a
    return table, reverse_table
ascii_table, ascii_table_reverse = make_conversion_tables()


def _build_ascii_regex():
    letters = list(ascii_consonants + ascii_vowels)
    for tone in tone_order:
        a = ascii_tones[tone]
        if a != '':
            letters.append(a)
    pattern = choices_pattern(letters, split=True)
    return re.compile(pattern, re.IGNORECASE)
ascii_regex = _build_ascii_regex()


def _build_unicode_regex():
    letters = list(unicode_consonants + unicode_vowels)
    for tone in tone_order:
        a = unicode_tones[tone]
        if a != '':
            letters.append(a)
    pattern = choices_pattern(letters, split=True)
    return re.compile(pattern, re.IGNORECASE | re.UNICODE)
unicode_regex = _build_unicode_regex()


def split_ascii(ascii_text):
    return [
        s for s in ascii_regex.split(ascii_text)
        if s != ''
    ]


def split_unicode(unicode_text):
    return [
        s for s in unicode_regex.split(unicode_text)
        if s != ''
    ]


def convert_ascii(ascii_text):
    return ''.join(
        ascii_table.get(s, s)
        for s in split_ascii(ascii_text)
    )


def convert_ascii_reverse(unicode_text):
    return ''.join(
        ascii_table_reverse.get(s, s)
        for s in split_unicode(unicode_text)
    )


def convert_ascii_to_html(ascii_text):
    unicode_text = convert_ascii(ascii_text)
    return unicode_text.replace('ʰ', (
        '<tspan baseline-shift="33.3%" font-size="58.3%" '
        'font-weight="bold">h</tspan>'
    ))
