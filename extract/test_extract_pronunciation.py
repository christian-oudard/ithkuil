# coding: utf8

import unittest
from unittest import TestCase

from pyquery import PyQuery as pq
from ithkuil.extract_pronunciation import extract, unwrap


class ExtractTest(TestCase):

    test_cases = [
        (
            '<strong><img src="images/5-10-33c.jpg" width="133" eight="46" /><br />\nAr-ryigrawilei&#355;rar&nbsp;  eglul&ocirc;n. </strong><span class="style25">&rarr; </span><strong>&Ccedil;tar-rya&nbsp; <strong>ei<strong><strong>r</strong></strong>&#355;</strong> <strong>&nbsp;</strong>igralar&nbsp;  eglul&ocirc;n.</strong>',
            [
                'Ar-ryigrawileiţrar\xa0  eglulôn.',
                'Çtar-rya\xa0 eirţ \xa0igralar\xa0  eglulôn.',
            ],
        ),
        (
            '<strong>ea<strong></strong></strong>',
            ['ea'],
        ),
        (
            '<strong>Auspal\xa0 ük<sup>h</sup>u\xa0 tô\xa0  myal. </strong>',
            ['Auspal\xa0 ükʰu\xa0 tô\xa0  myal.']
        ),
    ]
    def test_extract(self):
        for html, sentences in self.test_cases:
            self.assertEqual(
                [text for text, element in extract(pq(html))],
                sentences,
            )


class UnwrapTest(TestCase):
    def test_unwrap(self):
        before = '<p>one two <strong>three <em>four</em></strong></p>'
        after = '<p>one two three <em>four</em></p>'
        d = pq(before)
        t = d('strong')
        unwrap(t)
        self.assertEqual(str(d), after)


if __name__ == '__main__':
    unittest.main()
