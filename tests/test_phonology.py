from nose.tools import assert_equal

from ithkuil.phonology import split_ascii, convert_ascii


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
    for word, converted_word in cases:
        def test(word):
            assert_equal(convert_ascii(word), converted_word)
        yield test, word
