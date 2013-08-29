import re

from ithkuil.phonology import split_ascii
from .consonant import consonants
from .bottom_ending import bottom_endings, Normal as BottomNormal
from .side_ending import side_endings, Normal as SideNormal


# Tables.

basic_consonants = {
    cons.pronunciation: cons
    for cons in consonants
}


def _make_consonant_affixes():
    pairs = []
    for be in bottom_endings:
        pron = be.pronunciation
        if isinstance(pron, tuple):
            pairs.extend((p, be) for p in pron)
        else:
            pairs.append((pron, be))

    prefixes = {}
    suffixes = {}
    geminate_ending = None
    for pron, be in pairs:
        if pron == 'GEMINATE':
            geminate_ending = be
        elif pron.endswith('-'):
            pron = pron[:-1]
            prefixes[pron] = be
        elif pron.startswith('-'):
            pron = pron[1:]
            suffixes[pron] = be
    return prefixes, suffixes, geminate_ending
(
    consonant_prefixes,
    consonant_suffixes,
    geminate_ending,
) = _make_consonant_affixes()


transliteration_vowels = {
    se.transliteration: se
    for se in side_endings
}


def _make_consonant_clusters():
    result = []

    def assemble(cluster, affix, letter):
        if affix == 'GEMINATE':
            cluster = cluster + cluster
        elif affix.endswith('-'):  # Prefix.
            cluster = affix + cluster
        elif affix.startswith('-'):  # Suffix.
            cluster = cluster + affix
        result.append((cluster, letter))

    for cons in consonants:
        for be in bottom_endings:
            letter = cons(None, be)
            cluster = cons.pronunciation
            affix = be.pronunciation
            if not isinstance(affix, tuple):
                affix = (affix,)
            for a in affix:
                assemble(cluster, a, letter)

    return result

consonant_clusters = _make_consonant_clusters()


# Writing functions.

def write_consonant_cluster(text):
    pass


def write_transliteration(text):
    syllables = split_dotted_text(text)
    result = []
    for syllable in syllables:
        result.extend(write_transliteration_syllable(syllable))
    return result


def split_dotted_text(text):
    words = text.split()
    return list(flatten(w.split('.') for w in words))


def flatten(iterable):
    iterable = iter(iterable)
    stack = []
    while True:
        for item in iterable:
            if (
                hasattr(item, '__iter__') and
                not isinstance(item, (str, bytes))
            ):
                stack.append(iterable)
                iterable = iter(item)
                break
            else:
                yield item
        else:
            if not stack:
                return
            iterable = stack.pop()


def write_transliteration_syllable(text):
    return []
    #vowels_before, consonants, vowels_after = split_syllable(text)


def split_syllable(text):
    return
