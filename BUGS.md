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

- Affixual adjuncts elide their final vowel unconditionally, producing
  word-final clusters §4.2 of the phonotactics bars, and §4.2 is
  therefore not enforced either. Both halves are one fix.

  There is no defect in the sources here, though an earlier reading of
  this filed one. §8 is an inventory of what may *be* a root or affix;
  §4 constrains what may *end a word*. They are different axes, so
  **tr** being a permissible affix and -**tr** being barred word-finally
  are both true and do not conflict.

  The morphology supplies the repair. §4.1.1's shape is `'V_X C_S
  (V_S)` and §4.1.2's ends in `V_Z`, and those defaults are printed
  `(a)` in parentheses, the same notation Slot IX uses for THM: elidable,
  not obligatorily elided. §4.1.1's own examples show both shapes, *ač*
  and *aull* ending in consonants beside *etra*, *iakse* and *usmú*
  ending in vowels. `roman` elides regardless and then reports
  `xaheitr` unpronounceable, rather than writing the vowel it dropped.

  This is the Slot IX finding one slot over: an elidable default that
  must not be elided when eliding breaks phonotactics. §3.1.3 states the
  rule for Slot IX, "Before eliding the -**a**-, External Juncture
  requirements of Sec. 1.5 must be taken into account"; nothing states
  it for V_S or V_Z, but §4 stands where §1.5 does.

  Order matters when implementing: fix the renderer first, then enforce
  §4.2. Enforcing first breaks `TestToken_CorpusRoundTrip`,
  `TestMultipleAffixAdjunct_RoundTrip` and the classifier fuzzer, which
  is what happened when it was tried.

  Two things found while trying, so they need not be found again.
  `isFricative` in `phonotactics.go` covers only **f v ţ ḑ**, because it
  serves §2.5 and its subject is homologous pairs disagreeing in
  voicing; §4.2 means §1.1's whole Fricative row, and a narrow predicate
  rejects the attested `erčädókh` (-**kh**) and `mmiexinļ` (-**nļ**).
  And §4.2 must apply only to final conjuncts of exactly two consonants,
  per its own "-CC" heading, or it rejects the tri- and
  tetra-consonantal C_A complexes §4.3 and §4.4 permit.

  A speaker judged -**tr** "not that bad", so §4.2.1 may be stricter
  than the mouth requires. That does not change the fix: the repair
  exists and the renderer should use it either way.

  Enforcing §4.2 also trips `FuzzClassifyWord`, and that is a defect in
  the test rather than a question about the parser. Its assertion reads

      // Char validation is a hard precondition: anything holding a
      // non-V4 rune has to fail, never come back as some word class.
      charsOK := phonology.CheckText(in) == nil

  The comment says chars and the variable is named for chars, but
  `CheckText` judges the whole phonotactics, so the test demands that
  anything phonotactically ill-formed also fail to parse. That is the
  opposite of the documented architecture: reading is lenient and
  judging is separate, "because the C_A tables generate a few clusters
  our reading of §2 rejects and a parser that refused them could not
  round-trip its own output". Narrow the precondition to a chars-only
  check when §4.2 lands.

  The renderer half was written and reverted with the rest. Two things
  it established. V_Z cannot be restored as "a": an elided V_Z means
  "the same scope as C_Z", not a default value, so spelling it out
  writes the vowel naming that scope, and "a" would say VDom. V_S is not
  like that — §4.1.1's "(a)" really is the value — so the two slots need
  different treatment despite looking alike.
