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

## a modular adjunct's Slot 3 separator has no segment

§4.3 spends Slot 3's consonant entirely on C_M, "n if V_N represents an
Aspect, otherwise ň", where Slot 2 writes a full C_N. `view` derives
every entry's consonant from `slots.VnCnFromSlotVIII`, which answers
with a C_N, so a three-entry adjunct's phonetic column does not add up
to the word:

```
wähňainui   w- ä- hň- ai- ∅ ui- ∅      the n has no segment
```

The slot is shown as elided because the C_N it looked for is genuinely
not there. That much is honest, and better than the invented letters it
printed before, but the n that *is* there is still unaccounted for and
the label on the elided slot names the wrong thing.

The fix is to mirror §4.3's slots rather than treat every entry as a
formative-style (V_N, C_N) pair, which means the rule now lives in two
places: `roman.ModularAdjunct` writes it and `view.SegmentsModular`
would have to read it back. Better would be for the romanization arm to
hand out the split it already computed, which is a change to what
`roman` exposes rather than a repair here.

`view.TestSegmentsModular_SlotThreeSeparator`

## Words we cannot read

Two drift guards fail when the set changes in either direction, so they
are the live list rather than anything written here:

- `roman/corpus_test.go`, official examples that do not classify.
- the Discord word list, community words, where a leading `!`
  marks one we disagree with.
