# Open bugs

Defects in this code. Defects in Quijada's published sources go in
`docs/reference/issues.md` instead.

Each entry is a pointer, not the record. Where a bug has a skipped test,
that test holds the detail: the section it rests on, and why the obvious
fix is wrong. `go test ./... -v | grep SKIP` lists them directly.
Compose's 38 skipped subtests are not defects; they are corpus words
that are not formatives, skipped by design.

## compose reaches only formatives from the CLI

`cmd/ithkuil/compose.go` calls `compose.Formative` instead of
`compose.ParseToken`, so twelve of the thirteen word classes are
unreachable from the command line:

```
compose '1m-ERG'      ->  compose: root "1m": non-Ithkuil characters
compose '1m-ERG-CTE'  ->  compose: root "1m": non-Ithkuil characters
compose 'NOM:1m-ERG'  ->  compose: no root in "NOM:1m-ERG"
compose '[QUO]-ERG'   ->  compose: no root in "[QUO]-ERG"
compose 'DOL'         ->  compose: no root in "DOL"
```

A referential head is taken for a root cluster, and the error names a
root the expression never claimed to have. It used to be worse: `1m-ERG`
built `wa1mo`, with a digit inside an Ithkuil word, until "compose:
reject a root that cannot spell a word" stopped that.

The library is not at fault:
`compose.ParseToken` handles all thirteen classes and is tested. The
README documents the gloss syntax for every class, so for non-formative
words it documents something that cannot be run.

No skipped test yet. `compose/documented_syntax_test.go` tests the
library function, not the command, which is the gap that let this
through.

## render cannot write ëztewim

`render.Referential` picks the §4.6.1 Slot 3 w/y separator by validating
the prefix-less body, and only adds the epenthetic `-ë-` afterwards. For
a head cluster that needs the prefix, both candidates it weighs are
unpronounceable for the reason the prefix would have fixed, and the
render fails outright. §4.6.1's own example does not survive
read to write.

`render.TestReferential_EpentheticPrefixWithSecondReferent`

## render never writes the §1.7 Rule 1 glottal

§1.7 gives two placements for a case vowel's glottal stop, and the
renderer writes Rule 3's epenthetic spelling in every slot, including
the ones Rule 1 serves. §4.6.1's printed `fo'we'is` comes back
`fo'owe'is`, and `lai'wiš` comes back `la'iwiš`. Both re-parse to the
right value, so the round-trip closes over them and nothing else catches
it. The parse arm reads both placements.

`render.TestReferential_Rule1GlottalPlacement`

## validation enforces an unsourced "2.23"

The rule barring `ḑs`, `ḑš`, `ḑz`, `ḑž` and `nň` is in no published
document (`issues.md` G37). Its content is corroborated without
exception across all 5,946 roots and 528 affixes, which is what a real
rule from an unread source looks like and equally what an inference from
the lexicon looks like. The documents cannot separate the two, so it
stays enforced. `allomorph/substitutions.go` rests on the same decision.

`validation.TestCheckProhibitedPair_Rule223_IsUnsourced`

## fullparse cannot read an epenthetic ë inside a C_1 cluster

§4.6.1 puts the epenthetic vowel "before or within C_1 combinations",
and gives `zëmse` as its example. Only the leading position is read.

`fullparse.TestReferential_EpentheticVowelWithinC1`

## Words we cannot read

Two drift guards fail when the set changes in either direction, so they
are the live list rather than anything written here:

- `tokenize/corpus_test.go`, official examples that do not classify.
- `corpus/discord_examples.txt`, community words, where a leading `!`
  marks one we disagree with.
