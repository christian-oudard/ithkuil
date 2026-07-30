# Open bugs

Defects in this code. Defects in Quijada's published sources go in
`docs/reference/ISSUES.md` instead.

Each entry is a pointer, not the record. Where a bug has a skipped test,
that test holds the detail: the section it rests on, and why the obvious
fix is wrong. `go test ./... -v | grep SKIP` lists them directly.
Gloss's 38 skipped subtests are not defects; they are corpus words
that are not formatives, skipped by design.

## a default Slot VIII is two grammar values for one word

`grammar.Formative` admits both an absent Slot VIII and one holding the
defaults, and they are the same grammar: MNO Valence with FAC
Mood/Case-Scope is what a formative has when the slot is not there.
Nothing folds them together, so they get two romanizations:

```
SlotVIII: nil                        -> mlala
SlotVIII: VnCnValence{MNO, FAC}      -> mlalah
```

Both parse back to what they came from, so no round trip notices, and
the gloss arm disagrees with the romanization arm about it: the glosser
writes nothing for a default slot inside a formative, so both gloss to
`ml` and `gloss.ParseWord` gives back the absent one.

The fix is not on either arm. Two Go values for one grammatical state is
a fact about the type, and the check belongs there, either by making the
field unable to hold the defaults or by normalizing on construction.
Doing it in render alone would leave `grammar` still able to express the
distinction and every other caller still able to trip over it.

Found by `inventory`, whose sample for MNO carries the absent slot for
this reason: it is the one either arm produces.

## Words we cannot read

Two drift guards fail when the set changes in either direction, so they
are the live list rather than anything written here:

- `roman/corpus_test.go`, official examples that do not classify.
- the Discord word list, community words, where a leading `!`
  marks one we disagree with.
