# Open bugs

Defects in this code: places where it fails to do what we intend.

Two sibling files cover the language rather than the code.
`docs/reference/ERRATA.md` records every decision we have made about
what the sources mean, and every place our output departs from them.
`docs/reference/ISSUES.md` is the worklist of source defects we have
found but not yet decided about.

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

## the referents have no names or meanings

`la` glosses as `1m-THM`, and the detailed view now expands THM and not
`1m` — not because it does not try, but because there is nothing to
find. The eleven §4.6 referents and the three referent Effects carry no
name and no description anywhere: `search.Table` has no Referent
category, and `g.Name`/`g.Meaning` answer "" for every one of them.

```
1m   Name=""  Meaning=""          THM  Name="Thematic"  Meaning="inactive participant…"
```

So a referential, whose whole content is its referent, is the one class
whose main code cannot be explained by any mechanism. This is a hole in
`data/data.json` rather than in the code that reads it, which is why
expanding the codes did not close it.

`cmd/ithkuil/expand_test.go` covers the classes that do resolve.

## Words we cannot read

Two drift guards fail when the set changes in either direction, so they
are the live list rather than anything written here:

- `roman/corpus_test.go`, official examples that do not classify.
- the Discord word list, community words, where a leading `!`
  marks one we disagree with.

- Nothing enforces that a C_R root or C_S affix is a permissible root
  or affix form. §8 tabulates the bi-consonantal ones and §9 the
  tri-consonantal ones, and both are read by tests
  (`phonology/section8_test.go`, `section9_test.go`) but by no
  production code. A cluster is judged only by its adjacent pairs, so
  `ClusterLegal("fbm")` is true: `fb` and `bm` are each permissible
  per §8, while §9's row for medial **b** whose initials include **f**
  permits only `vlrwyř` third, which excludes **m**. A speaker reported
  `fbm` impossible to say.

  Scope matters and the first diagnosis of this got it wrong. §9 is
  titled "Permissible Tri-Consonantal Conjuncts Which Can Be Roots or
  Affixes", so it does not govern every triple in a word: a Slot VI C_A
  complex is tri-consonantal and answers to §4.3 and §4.4 instead.
  Applying §9 inside `ClusterLegal` would therefore reject legitimate
  C_A forms and break round-tripping, which is the failure mode CLAUDE.md
  already warns about for §2. The check belongs where a root or affix is
  built or read, and needs the §9 table embedded rather than parsed from
  the reference document at runtime.

  Neither table can currently fail in the direction that matters:
  `section9_test.go` expands the whitelist and asserts each entry is
  legal, which tests what it admits and never what it excludes.
