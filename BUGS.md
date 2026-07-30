# Open bugs

Defects in this code. Defects in Quijada's published sources go in
`docs/reference/ISSUES.md` instead.

Each entry is a pointer, not the record. Where a bug has a skipped test,
that test holds the detail: the section it rests on, and why the obvious
fix is wrong. `go test ./... -v | grep SKIP` lists them directly.
Gloss's 38 skipped subtests are not defects; they are corpus words
that are not formatives, skipped by design.

## a concatenated chain cannot be composed back into a chain

The canonical gloss of a `grammar.Chain` is its members separated by a
space, so it is not one whitespace-delimited token and `gloss.ParseWord`
never sees it. `gloss.ParseText` reads the members as independent
formatives, and the chain they belonged to is gone:

```
hakšiţé-alcialu'a
  -> T1-ksq-STA.OBJ.EXS-MDS-COR lc-STA.BSC.RPS-NAV
  -> two grammar.Formative, not one grammar.Chain
```

Nothing is lost from the members themselves, the T1 marker survives on
the first, so what is missing is the reassembly. Every other word class
round-trips through compose; this is the one that does not.

Both fixes are real changes rather than repairs. Giving chains their own
separator in the gloss costs a mark, and the obvious one is taken: "-"
already separates slots, which is why the space was reached for. Teaching
`ParseText` to reassemble is the other, and it has to decide what a space
means everywhere else in the gloss before it can mean "next chain member"
here.

`gloss/corpus_gloss_test.go`'s `TestCorpusGloss_ComposesBack` holds the
count of chains at nineteen, so the gap cannot widen quietly.

## roman never writes the §1.7 Rule 1 glottal

§1.7 gives two placements for a case vowel's glottal stop, and the
renderer writes Rule 3's epenthetic spelling in every slot, including
the ones Rule 1 serves. §4.6.1's printed `fo'we'is` comes back
`fo'owe'is`, and `lai'wiš` comes back `la'iwiš`. Both re-parse to the
right value, so the round-trip closes over them and nothing else catches
it. The parse arm reads both placements.

`roman.TestReferential_Rule1GlottalPlacement`

## phonology enforces an unsourced "2.23"

The rule barring `ḑs`, `ḑš`, `ḑz`, `ḑž` and `nň` is in no published
document (`ISSUES.md` G37). Its content is corroborated without
exception across all 5,946 roots and 528 affixes, which is what a real
rule from an unread source looks like and equally what an inference from
the lexicon looks like. The documents cannot separate the two, so it
stays enforced. `allomorph/substitutions.go` rests on the same decision.

`phonology.TestCheckProhibitedPair_Rule223_IsUnsourced`

## roman cannot read an epenthetic ë inside a C_1 cluster

§4.6.1 puts the epenthetic vowel "before or within C_1 combinations",
and gives `zëmse` as its example. Only the leading position is read.

`roman.TestReferential_EpentheticVowelWithinC1`

## a modular adjunct's gloss does not compose back

Every distinct modular gloss in the corpus fails to read back, so the
class is write-only through the gloss arm:

```
a           RTR                        no root in "RTR"
ä           PRS                        no root in "PRS"
wähňainui   PRL.HYP-RSM-IRP-{parent}   root "{parent}": non-Ithkuil characters
```

Three separate holes, not one. A lone-aspect modular glosses to a bare
category abbreviation, which `looksLikeModular` accepts only when a
scope or reach tail was stripped first, so it falls through to the
formative parser. A multi-pair modular has hyphen-separated slots and
the scope tail is trimmed before the body is split, leaving `{parent}`
looking like another slot. An all-default modular glosses to `MOD`,
which composes to a value the renderer then refuses because §4.3 Slot
4 is mandatory — the two directions disagree about whether such a word
exists.

Widening the recogniser is not enough on its own: a bare `RTR` is
shape-identical to a bias or register abbreviation, and SPEC's
one-job-per-mark rule says a token's kind should follow from its shape
rather than from consulting three inventories in order.

`gloss.TestModularAdjunct_GlossDoesNotCompose`

## Words we cannot read

Two drift guards fail when the set changes in either direction, so they
are the live list rather than anything written here:

- `roman/corpus_test.go`, official examples that do not classify.
- `corpus/discord_examples.txt`, community words, where a leading `!`
  marks one we disagree with.
