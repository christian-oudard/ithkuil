# Open bugs

Defects in this code. Defects in Quijada's published sources go in
`docs/reference/ISSUES.md` instead.

Each entry is a pointer, not the record. Where a bug has a skipped test,
that test holds the detail: the section it rests on, and why the obvious
fix is wrong. `go test ./... -v | grep SKIP` lists them directly.
Gloss's 38 skipped subtests are not defects; they are corpus words
that are not formatives, skipped by design.

## phonology enforces an unsourced "2.23"

The rule barring `ḑs`, `ḑš`, `ḑz`, `ḑž` and `nň` is in no published
document (`ISSUES.md` G37). Its content is corroborated without
exception across all 5,946 roots and 528 affixes, which is what a real
rule from an unread source looks like and equally what an inference from
the lexicon looks like. The documents cannot separate the two, so it
stays enforced. `allomorph/substitutions.go` rests on the same decision.

`phonology.TestCheckProhibitedPair_Rule223_IsUnsourced`

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
