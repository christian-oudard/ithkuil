#!/usr/bin/env python3
"""Next headwords to write up in docs/dictionary/english.md.

Reads the frequency list, drops what the dictionary does not want, and
prints what is left in frequency order together with the evidence needed
to write the entry: candidate roots from the lexicon, and real
collocations from the Tatoeba corpus.

Dropped: closed-class words, which are grammar rather than vocabulary;
inflected forms whose lemma is already in the list; and anything a
heading in english.md already covers.

Usage:
  tools/worklist.py [-n N] [--collocates] [WORD ...]

With WORD arguments, reports on those words instead of the next N.
"""

import argparse
import collections
import csv
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DATA = ROOT / "data"
DOC = ROOT / "docs" / "dictionary" / "english.md"
REF = Path.home() / ".local/share/ithkuil/reference"

# Closed-class English: determiners, pronouns, prepositions,
# conjunctions, auxiliaries, modals, and the pro-forms. These are
# grammatical categories in Ithkuil, not lexical entries, and belong to
# a grammar guide instead.
CLOSED = set("""
a an the this that these those each every some any no none other another such
i me my mine myself you your yours yourself yourselves he him his himself
she her hers herself it its itself we us our ours ourselves they them their
theirs themselves who whom whose which what whatever whoever whichever
one ones oneself something someone somebody anything anyone anybody
everything everyone everybody nothing nobody thing things
am is are was were be been being do does did done doing have has had having
will would shall should can could may might must ought need dare used
of in on at by for with from to into onto upon over under above below
between among through during before after since until while within without
across against along around behind beneath beside beyond down off out
toward towards up via per about
and or but nor so yet if then than as because although though unless
whether however therefore thus moreover furthermore nevertheless
not no yes yeah yep nope
here there where when why how ever never always sometimes often seldom
very too quite rather almost just only also even still already yet again
more most less least much many few fewer several both all half whole
first second third fourth fifth last next
please thanks thank hello hi bye ok okay well erm um uh oh ah eh
mr mrs ms dr st jan feb mar apr jun jul aug sep oct nov dec pm am
""".split())

IRREGULAR = {
    "was": "be", "were": "be", "been": "be", "am": "be", "are": "be", "is": "be",
    "had": "have", "has": "have", "said": "say", "went": "go", "came": "come",
    "took": "take", "made": "make", "gave": "give", "knew": "know",
    "known": "know", "saw": "see", "seen": "see", "found": "find",
    "got": "get", "gotten": "get", "children": "child", "men": "man",
    "women": "woman", "began": "begin", "become": "become", "became": "become",
    "thought": "think", "told": "tell", "felt": "feel", "left": "leave",
    "held": "hold", "kept": "keep", "meant": "mean", "sent": "send",
    "spent": "spend", "brought": "bring", "bought": "buy", "caught": "catch",
    "taught": "teach", "sold": "sell", "told": "tell", "wrote": "write",
    "written": "write", "ran": "run", "sat": "sit", "stood": "stand",
    "lost": "lose", "won": "win", "paid": "pay", "put": "put", "read": "read",
    "heard": "hear", "led": "lead", "fell": "fall", "grew": "grow",
    "drew": "draw", "threw": "throw", "spoke": "speak", "broke": "break",
    "chose": "choose", "drove": "drive", "rose": "rise", "wore": "wear",
    "feet": "foot", "teeth": "tooth", "people": "person", "lives": "life",
}


def lemma_candidates(w):
    """Guessed base forms of w, from regular suffix stripping.

    Irregulars are not guesses and are handled separately, since the
    frequency test the guesses have to pass would reject them: "said"
    outranks "say" in running text, but is still its inflection.
    """
    out = set()
    for suf, repl in SUFFIXES:
        if w.endswith(suf) and len(w) - len(suf) >= 3:
            out.add(w[: len(w) - len(suf)] + repl)
    return out


# Inflectional: the result is the same word, so folding is safe whenever
# the result is a word at all.
INFLECTION = [("ies", "y"), ("es", ""), ("s", ""), ("ied", "y"),
              ("ed", ""), ("ed", "e"), ("ing", ""), ("ing", "e"),
              ("ly", ""), ("ies", "ie")]

# Derivational: the result is a different word, and stripping can land
# on an unrelated one. "number" is not an inflection of "numb". These
# only fold when the candidate is the more frequent of the two.
DERIVATION = [("er", ""), ("er", "e"), ("est", ""), ("est", "e"),
              ("en", "")]

SUFFIXES = INFLECTION + DERIVATION


def vetted():
    """Words a learner's dictionary vouches for, from ECDICT.

    The frequency list is raw corpus counts, so its upper reaches carry
    proper nouns and transcription noise: London, John, British, erm.
    ECDICT's Oxford 3000 membership and Collins star rating are editorial
    judgements about which words are worth teaching, which is the same
    judgement this file needs. Absent the file, no filtering.
    """
    path = REF / "ecdict.csv"
    if not path.exists():
        return None
    csv.field_size_limit(10 ** 7)
    out = set()
    with open(path, encoding="utf-8") as f:
        for r in csv.DictReader(f):
            if r["oxford"] == "1" or (r["collins"] or "0") >= "3":
                out.add(r["word"].lower())
    return out


def covered():
    """Headwords already written up, from the '## ' headings."""
    if not DOC.exists():
        return set()
    out = set()
    for line in DOC.read_text().splitlines():
        if line.startswith("## "):
            for part in line[3:].split(","):
                out.add(part.strip().lower())
    return out


def roots():
    """Root cluster -> the four stem glosses."""
    with open(DATA / "roots.tsv", encoding="utf-8") as f:
        cols = ["Stem 0 / Basic", "Stem 1", "Stem 2", "Stem 3"]
        return [(r["Root"], [r[c] for c in cols])
                for r in csv.DictReader(f, delimiter="\t")]


WORD_RE = re.compile(r"(?<![\w-])%s(?![\w-])")


def root_hits(word, table, limit=3):
    """Roots naming word as a whole word in any stem, best first.

    Ranked by how much of the gloss is this word. A root whose Stem 0
    reads "water" is about water; one whose Stem 2 mentions water inside
    forty words of fish taxonomy is not, and the species glosses are
    long enough that length alone separates them well.
    """
    pat = re.compile(r"(?<![\w-])" + re.escape(word) + r"(?![\w-])", re.I)
    hits = []
    for cr, stems in table:
        best = None
        for i, gloss in enumerate(stems):
            m = gloss and pat.search(gloss)
            if not m:
                continue
            # Earlier in the gloss and shorter overall both mean the
            # gloss is more nearly about this word.
            score = m.start() + len(gloss)
            if best is None or score < best[0]:
                best = (score, i)
        if best:
            hits.append((best[0], cr, stems))
    hits.sort()
    return [(cr, stems) for _, cr, stems in hits[:limit]]


def collocates(word, limit=12):
    """What the word is actually used with, from Tatoeba."""
    path = REF / "eng_sentences.tsv"
    if not path.exists():
        return []
    pat = re.compile(r"\b" + re.escape(word) + r"\s+([a-z]+)", re.I)
    c = collections.Counter()
    with open(path, encoding="utf-8") as f:
        for line in f:
            p = line.split("\t")
            if len(p) > 2:
                for m in pat.finditer(p[2]):
                    c[m.group(1)] += 1
    return [(w, n) for w, n in c.most_common(limit * 4)
            if w not in CLOSED][:limit]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("words", nargs="*")
    ap.add_argument("-n", type=int, default=40)
    ap.add_argument("--collocates", action="store_true")
    args = ap.parse_args()

    table = roots()

    if args.words:
        targets = args.words
    else:
        freq = [w.strip().lower() for w in
                open(REF / "common_words_50k", encoding="utf-8") if w.strip()]
        # Fold each word onto its lemma so "years" and "said" are
        # written up once, as "year" and "say". Only fold onto a
        # candidate that is *more* frequent than the word itself: a real
        # lemma nearly always outranks its own inflected form, whereas
        # the accidents this rule has to reject do not. Stripping "-er"
        # off "number" yields "numb", a real word but a far rarer one.
        rank = {w: i for i, w in enumerate(freq)}
        ok = vetted()
        seen, done, targets = set(), covered(), []
        for w in freq:
            if not w.isalpha() or w in CLOSED:
                continue
            w = IRREGULAR.get(w, w)
            for suffixes, needs_rank in ((INFLECTION, False), (DERIVATION, True)):
                hit = None
                for suf, repl in suffixes:
                    if not w.endswith(suf) or len(w) - len(suf) < 2:
                        continue
                    cand = w[: len(w) - len(suf)] + repl
                    if needs_rank:
                        if rank.get(cand, len(rank)) < rank[w]:
                            hit = cand
                    elif ok and cand in ok:
                        hit = cand
                    if hit:
                        break
                if hit:
                    w = hit
                    break
            if w in CLOSED or w in seen or w in done:
                continue
            if ok and w not in ok:
                continue
            seen.add(w)
            targets.append(w)
            if len(targets) >= args.n:
                break

    for w in targets:
        print(f"## {w}")
        for cr, stems in root_hits(w, table):
            print(f"   -{cr}-  " + " | ".join(stems))
        if args.collocates:
            co = collocates(w)
            if co:
                print("   uses: " + ", ".join(f"{x}({n})" for x, n in co))
        print()


if __name__ == "__main__":
    main()
