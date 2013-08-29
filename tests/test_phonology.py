from nose.tools import assert_equal

from ithkuil.phonology import (
    split_ascii,
    split_unicode,
    convert_ascii,
    convert_ascii_reverse,
)


def test_split_ascii():
    cases = [
        ("Ikc,as", ['I', 'k', 'c,', 'a', 's']),
        ("=Sakc^'a", ['=', 'S', 'a', 'k', "c^'", 'a']),
        # Also works for transliterations.
        ('kaet', ['k', 'ae', 't']),
        ('no%t', ['n', 'o%', 't']),
    ]
    for word, split_word in cases:
        def test(word):
            assert_equal(split_ascii(word), split_word)
            assert_equal(
                [
                    convert_ascii_reverse(letter) for letter in
                    split_unicode(convert_ascii(word))
                ],
                split_word
            )
        yield test, word


def test_split_unicode():
    cases = [
        ('âpcââl', ['â', 'p', 'c', 'ââ', 'l']),
    ]
    for word, split_word in cases:
        def test(word):
            assert_equal(split_unicode(word), split_word)
        yield test, word


def test_convert_ascii():
    cases = [
        ("Ikc,as", 'Ikças'),
        ("A'tukc,as te^ oxnall", 'A’tukças tê oxnall'),
        ("U^b eikkradwa smou'ola^xh.", 'Ûb eikkradwa smou’olâxh.'),
        ("=Sakc^'a to^ myicka zboack.", '¯Sakč’a tô myicka zboack.'),
        ("o\spa^tlo:k", 'òspâtlök'),
        ("Aigwapskh eks^u/ll,", 'Aigwapskʰ ekšúlļ'),
        ("a^pca^/l", 'âpcââl'),
        ("a^pca^a^l", 'âpcââl'),
        ('kaet', 'kæt'),
        ('no%t', 'nøt'),
        ('dzi+', 'żɨ'),
    ]
    for ascii_word, unicode_word in cases:
        def test(ascii_word):
            assert_equal(convert_ascii(ascii_word), unicode_word)
        yield test, ascii_word


def test_convert_ascii_reverse():
    cases = [
        ('Ikças', "Ikc,as"),
        ('A’tukças tê oxnall', "A'tukc,as te^ oxnall"),
        ('Ûb eikkradwa smou’olâxh.', "U^b eikkradwa smou'ola^xh."),
        ('¯Sakč’a tô myicka zboack.', "=Sakc^'a to^ myicka zboack."),
        ('òspâtlök', "o\spa^tlo:k"),
        ('Aigwapskʰ ekšúlļ', "Aigwapskh eks^u/ll,"),
        ('âpcââl', "a^pca^/l"),
        ('kæt', 'kaet'),
        ('nøt', 'no%t'),
        ('żɨ', 'dzi+'),
    ]
    for unicode_word, ascii_word in cases:
        def test(unicode_word):
            assert_equal(convert_ascii_reverse(unicode_word), ascii_word)
        yield test, unicode_word
