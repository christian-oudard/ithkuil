# Open bugs

Defects in this code. Defects in Quijada's published sources go in
`docs/reference/ISSUES.md` instead.

Each entry is a pointer, not the record. Where a bug has a skipped test,
that test holds the detail: the section it rests on, and why the obvious
fix is wrong. `go test ./... -v | grep SKIP` lists them directly.
Compose's 38 skipped subtests are not defects; they are corpus words
that are not formatives, skipped by design.

## Four word classes cannot be written back

`tokenize.Render` dispatches over the whole word-class sum and answers
for nine of the thirteen. Modular adjuncts, the two affixual adjuncts
and parsing adjuncts fall through to "no renderer for ...", because the
`render` package has no function for any of them: it covers formatives
and the two referentials, and everything else is a table lookup in
`grammar` that these four have no equivalent of.

```
compose 'MOD'    ->  compose: no renderer for tokenize.ModularWord
compose 'mono:'  ->  compose: no renderer for tokenize.ParsingAdjunctWord
```

The parse arm reads all four, so this is one-way: a modular adjunct in
a sentence glosses correctly and cannot be composed. Nothing in the
grammar makes them unwritable — §4.2's V_N/C_N shape and §4.7's stress
markers are as mechanical as the rest — the code was simply never
written.

`cmd/ithkuil/main_test.go`'s `TestCompose_NoRenderer`

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

## phonology enforces an unsourced "2.23"

The rule barring `ḑs`, `ḑš`, `ḑz`, `ḑž` and `nň` is in no published
document (`ISSUES.md` G37). Its content is corroborated without
exception across all 5,946 roots and 528 affixes, which is what a real
rule from an unread source looks like and equally what an inference from
the lexicon looks like. The documents cannot separate the two, so it
stays enforced. `allomorph/substitutions.go` rests on the same decision.

`phonology.TestCheckProhibitedPair_Rule223_IsUnsourced`

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
