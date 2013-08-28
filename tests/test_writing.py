from nose.tools import assert_equal

from ithkuil.writing import (
    split_dotted_text,
    write_consonant_cluster,
    write_transliteration,
)


def test_split_dotted_text():
    assert_equal(
        split_dotted_text('ap.ple ba.na.na grape o.range'),
        ['ap', 'ple', 'ba', 'na', 'na', 'grape', 'o', 'range']
    )


def test_write_consonant_cluster():
    assert_equal(
        str(write_consonant_cluster('p')[0]),
        'P(side=Normal, bottom=Normal)'
    )


def test_transliteration():
    write_transliteration('jo.n ki.ha.da')
    assert False  # STUB
