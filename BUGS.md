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

## Words we cannot read

Two drift guards fail when the set changes in either direction, so they
are the live list rather than anything written here:

- `roman/corpus_test.go`, official examples that do not classify.
- the Discord word list, community words, where a leading `!`
  marks one we disagree with.

- Nothing enforces that a C_R root or C_S affix is a permissible root
  or affix form. The check now exists — `phonology.RootConjunctLegal`,
  §§9-11 embedded as rows and held against the reference document by
  `TestRootConjunctRows_MatchTheDocument` — but no production code
  calls it. `ClusterLegal("fbm")` is still true, since a cluster is
  judged only by its adjacent pairs: `fb` and `bm` are each permissible
  per §8, while §9's row for medial **b** whose initials include **f**
  permits only `vlrwyř` third, which excludes **m**. A speaker reported
  `fbm` impossible to say.

  What stops enforcement is what the check found. 453 of the community
  lexicon's 5,895 roots are shapes §§9-11 do not admit, in families
  rather than scattered — `-pf-` after fourteen initials §9 does not
  give it, `Cml`/`Cmr`/`Cmř`, and two affixes ending `-ḑr`. Refusing 8%
  of the vocabulary is not a call to make here; the finding is filed as
  `ISSUES.md` L1 and the counts are pinned by
  `lexicon/root_shape_test.go`, so the shape of the gap cannot change
  unnoticed. Every root of one or two consonants passes, which is the
  split that makes it a real finding: §8 is derived from the pair rules
  and §§9-11 are not derived from anything.

  Scope matters and the first diagnosis of this got it wrong. §9 is
  titled "Permissible Tri-Consonantal Conjuncts Which Can Be Roots or
  Affixes", so it does not govern every triple in a word: a Slot VI C_A
  complex is tri-consonantal and answers to §4.3 and §4.4 instead.
  That is why `RootConjunctLegal` is a separate predicate rather than
  part of `ClusterLegal`, which would reject legitimate C_A forms and
  break round-tripping.

  The one-directional test is fixed. `section9_test.go` expands the
  whitelist and asserts each entry is legal, which tests what the table
  admits and never what it excludes;
  `TestRootConjunctLegal_Excludes` covers the other direction.
