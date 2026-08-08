# Reading the reference documents

Two facts about these transcriptions that a section number will not
tell you.

## The website is a third document, not a renumbering

`https://ithkuil.net/` carries "A GRAMMAR OF NEW ITHKUIL" in fourteen
chapters: Phonology, Morpho-Phonology, Basic Morphology, Case
Morphology, Verb Morphology, More Verb Morphology, Affixes, Adjuncts,
Referentials, Special Constructions, Syntax, The Writing System,
Numbers, The Lexicon. The PDF this directory transcribes is "NEW
ITHKUIL: GRAMMAR DESIGN". They are separate documents covering
overlapping ground, so their section numbers coincide only by accident.

Where they coincide they really do. The site's chapter 1 runs 1.0
PHONOLOGY, 1.1 Phonemic Inventory, 1.2 Pronunciation Notes and
Allophonic Distinctions, 1.2.1 Vowels, matching the PDF exactly, and
its inventory chart has the same thirteen columns in the same order,
labial through lateral. That is a second witness to §1.1's layout,
which matters because our transcription of that chart had several rows
shifted a column left.

Where they diverge, they diverge silently. What the PDF calls §5.6,
WH-interrogatives, sits at 10.6 on the site, inside Special
Constructions, and 5.6 on the site holds aspect examples instead. See
the head of `code/corpus/examples.txt`.

No mapping between the two has been made. A section number taken from
the site should not be cited against this directory without checking
which document it came from.

## Section numbers do not identify a rule on their own

The Grammar Design document and the phonotactics document number
independently and collide throughout:

| number | morphology | phonotactics |
|---|---|---|
| 1.5 | External Juncture | glottal-stop placement |
| 1.6 | the Standard Vowel-Form Sequence | final -h before initial h- |
| 1.7 | inserting a glottal stop into a vowel-form | geminates |
| 4.2 | Slot IV V_R for the Specialized C_S-Root | word-final bi-consonantal conjuncts |

Both collisions have caused real confusion here. §1.5 was read as the
juncture rule when the glottal-stop rule was meant. And the §4.2 entry
in ISSUES.md concerns the phonotactics rule, while the version history
above records a v1.3 change to "Sec. 4.2" that is the morphology one:
two unrelated rules, one label, one directory.

So a bare section number is ambiguous, in prose and in a fault code
alike. The convention the code follows: a `fault.Code` citation names
its document outright, since it is a bare label with no sentence around
it to place it (`phonotactics §2.16`, `grammar §1.2.1`); a citation
inside a `Fix` sentence names the phonotactics document when it means
that one, and otherwise means the Grammar Design document, which is
what the sentence around it is already about.
`code/fault/citation_test.go` holds every shipped message to this, and
to the stronger claim that the section cited exists at all.

Comments are not held to it, and inside `code/phonology` a bare §2.13
means the phonotactics rule: that package is a transcription of that
document and reads as one. What a comment can assume of its reader, a
sentence printed to someone with neither document open cannot.

The website does not have this problem, being one document in one
sequence, but it is not a substitute: the site lags the PDFs, so it
corroborates rather than supersedes. It is most useful for material
unlikely to have been revised, such as the phoneme inventory.

