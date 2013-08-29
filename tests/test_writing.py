from nose.tools import assert_equal
from unittest.case import SkipTest

from ithkuil.api import consonant, primary
from ithkuil.writing import (
    split_dotted_text,
    write_consonant_cluster,
    write_transliteration,
)


def test_character_str():
    assert_equal(
        str(consonant('P', 'Normal', 'Normal')),
        'P(Normal, Normal)'
    )
    assert_equal(
        str(primary('Oblique', 'Normal', 'Normal')),
        'Oblique(Normal, Normal)'
    )


def test_split_dotted_text():
    assert_equal(
        split_dotted_text('ap.ple ba.na.na grape o.range'),
        ['ap', 'ple', 'ba', 'na', 'na', 'grape', 'o', 'range']
    )


def test_write_consonant_cluster():
    raise SkipTest()
    assert_equal(
        str(write_consonant_cluster('p')[0]),
        'P(side=Normal, bottom=Normal)'
    )
    assert_equal(
        str(write_consonant_cluster('lp')[0]),
        'P(side=Normal, bottom=Long)'
    )


def test_transliteration():
    raise SkipTest()
    write_transliteration('jo.n ki.ha.da')
