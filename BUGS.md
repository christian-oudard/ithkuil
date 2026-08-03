# Open bugs

Defects in this code. Defects in Quijada's published sources go in
`docs/reference/ISSUES.md` instead.

Each entry is a pointer, not the record. Where a bug has a skipped test,
that test holds the detail: the section it rests on, and why the obvious
fix is wrong. `go test ./... -v | grep SKIP` lists them directly.
Gloss's 38 skipped subtests are not defects; they are corpus words
that are not formatives, skipped by design.

## §1.3.1's grave accent is read but never written

`Normalize` folds the grave off the -i- of a -Cìa- conjunct, so
karésìa, vélkìo and ehùá parse. Nothing puts it back, so a word read
from the grammar document comes back spelled differently from how the
document spells it.

What is unsettled is whether it belongs in canonical output at all.
§1.3.1 says the grave "is used" over -i- but only that it "may
similarly be used" over -u-, and Quijada never writes it in a native
formative: 100 words in the published corpus have the exact -Ci+V-
shape the rule describes and none carries the mark, while the grammar
document's eight uses are §1.3.1's own two demonstrations, the one -u-
example, and five §7 foreign place-names. Emitting it would produce
forms unlike anything attested; not emitting it contradicts §1.3.1 as
written. See the skipped `TestApply_GraveOnUnstressedI` in
`phonology/stress_test.go` for why putting it in `Apply` would be the
wrong place either way.

## Words we cannot read

Two drift guards fail when the set changes in either direction, so they
are the live list rather than anything written here:

- `roman/corpus_test.go`, official examples that do not classify.
- the Discord word list, community words, where a leading `!`
  marks one we disagree with.
