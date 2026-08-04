# Errata to the V4 Source Material

What Quijada said, and what we do instead.

The reference documents in this directory are transcriptions, and the
language they describe is his. Where they diverge, this file is the
record of why, entry by entry, so that the two can always be told
apart. Nothing is corrected silently and nothing is decided twice.

An entry belongs here when a reader of `morphology.md` or
`phonotactics.md` would otherwise be unable to reproduce our output.
That covers three cases:

- **Corrections.** The source prints something wrong and the reference
  document now prints something else.
- **Readings.** The source contradicts itself, or leaves a choice
  unstated, and an implementation cannot proceed without picking one.
- **Proposals.** An amendment we think the language wants, offered to
  Quijada and the community, not yet adopted here.

Each entry carries **Source**, **Decision**, **Status** and **Where**.
Entries are self-contained: nothing here needs another file to be
understood.

An entry is named for the section it rules on, not numbered. A serial
tells you nothing about what it says, collides when two people add one
at once, and has to be looked up; a section reference is what a reader
arrives with. Code cites an entry the same way — `see ERRATA.md §2.10`
— and a test checks the citation finds it.

## Status values

| | |
|---|---|
| `adopted` | in force; the reference documents and the code both follow it |
| `proposed` | offered as an amendment; we still implement the source as written |
| `implemented` | a proposal we have adopted ahead of the source |

## Relationship to ISSUES.md

`ISSUES.md` is a worklist, not a record. It holds defects in the
published sources that we have found but not yet disposed of. Working
an entry means deciding what to do about it, which produces an entry
here; the ISSUES entry then goes away. When that list reaches zero it
will be deleted and this file will be the only one left.

So: a defect with no decision is in `ISSUES.md`. A defect with a
decision is here. `BUGS.md` remains separate and is about our own code
failing to do what we intend, not about the language.

---

# Corrections

*Where the reference documents depart from the printed source.*

None yet. The transcriptions currently match their sources exactly,
verified in both directions for every Ithkuil form and sentence by
sentence for the prose. Entries arrive here as `ISSUES.md` is worked
through.

---

# Readings

*Where the source cannot be implemented as written, and we chose.*

### §2.10 — ç before a voiced sibilant affricate

**Source.** §2.10 of the phonotactics document bars **ç** from being
"followed by a voiced sibilant affricate (**ẓ**, **j**)", and gives a
phonetic reason. §8's grid of permissible bi-consonantal conjuncts
marks **çẓ** and **çj** permissible.

This is the whole of the disagreement between §8 and §§1-7. The other
26 rows of the grid regenerate from our reading of the rules cell for
cell, 810 cells in all, which is why the two cells stand out.

**Decision.** Follow §2.10. We bar both conjuncts. The prose states a
reason and the grid states none, and a grid is the kind of artefact
that is derived from rules rather than the other way round.

**Status.** `adopted`

**Where.** `phonology/section8_test.go:34`, `section8ExpectedDiff`,
which regenerates the whole grid and pins these two cells as the only
departures.

### §4.7 — the adjunct tables outrank the phonotactics

**Source.** Two documents disagree about what may begin a word. The
grammar document publishes tables of adjuncts and worked examples using
them; the phonotactics document rules that many of those forms cannot
open a word. Taking §3's word-initial inventory at its word, 34 of the
61 bias adjuncts are unlicensed: §3.2.9 grants word-initial **l-** and
**r-** only **-w** or **-y**, which fails ACC **lf** and eleven more;
§3.2.8 grants nasals a liquid or approximant, which fails ATE **ňj**
and ten more; **ř-** is granted no word-initial pair at all.

**Decision.** The adjunct tables are authoritative and §3's
word-initial rules are scoped to words that have a vowel in them. Every
rule in §3 is written about a conjunct with a vowel-form beside it —
§4.1 opens "A single word-final consonant following a vowel-form" — and
a bias adjunct is a bare consonant conjunct standing alone, a shape
those sections never contemplate. None of the forms is hypothetical:
**pļļ** is attested 291 times in the community corpus, **msf** 127,
**kçç** 48.

**Status.** `adopted`

**Where.** `phonology/word.go:248`, in `clusterViolations`, which
exempts single-conjunct consonant words from cluster validation.

### §3.8 — Pattern-2 FAC is written w

**Source.** §3.8's Pattern-2 Mood/Case-Scope table prints the FAC value
as "w/y" and gives no rule anywhere for choosing between them.

Nor is one derivable from usage: *arţtuläwá* and *erčuläyá* differ only
in the glide, with the same Ca, Vn and Vk. The official examples split
51 **w** to 12 **y** over 63 instances and the community corpus 415 to
30 over 445. Both appear after every plain vowel and several
diphthongs, and **-ou-** inverts the ratio outright at 12 **y** to 7
**w**. So nothing phonological conditions it and the minority form is
too common to be a slip.

**Decision.** Render **w**, the majority form. Parse both.

**Status.** `adopted`

**Where.** `slots/grammar.go:782`, `moodCnP2Table`; the parser accepts
**y** wherever it accepts **w** in `parse/slot_viii.go`.

### §4.6 — referential category affixes take the first permissible spelling

**Source.** §4.6 says to add the AGGLOMERATIVE, NOMIC or ABSTRACT affix
"immediately preceding or following" the referential "as
phonotactically permissible". That usually settles it on its own: of
*lça*, *lxa*, *çla* and *xla*, only the two prefixed forms are clusters
Ithkuil lets a word open with. Where more than one spelling survives,
the section does not choose.

**Decision.** Take the first spelling §4.6 lists, its own order being
the only ranking on offer. This is our ranking, not Quijada's: *çla*
and *xla* are both legal, as are *tļma*, *mtļa* and *ļma*, and the
canonicalization heuristics in `SPEC.md` have nothing to say here, the
candidates being identical in syllable count, glottal count and length.

**Status.** `adopted`

**Where.** `roman/referential.go:186`, `categoryForm`.

### §1.6 — glide dissimilation applies to a glottalized vowel

**Source.** §1.6's footnote says a Series-3 vowel-form beginning with
**-i** takes its alternate spelling after **y-**, and one beginning
with **-u** takes its alternate after **w-**: *yuä* not *yia*, *wiä*
not *wua*. Its examples are all bare vowel-forms. It does not say what
happens when the vowel carries a §1.7 glottal-stop.

The case is reachable. §3.5.1 forces a glottal-stop into Slot II when
Slot V holds two or more affixes, and a **y-** or **w-** shortcut in
Slot I puts that vowel directly after the glide.

**Decision.** Dissimilate. *yi'a* becomes *yu'ä* for the same reason
*yia* becomes *yuä*.

The footnote is about which spelling of a vowel-form to write. The
glottal-stop is not part of the form: §3.9.1's SPECIAL NOTE lets it
move onto a different slot's vowel entirely, which it could not do if
it were. So the stop is a marker docked onto whichever spelling the
footnote selects, and the glide still sits against the matching vowel,
which is the whole of what the rule is about. Declining to dissimilate
here would invent an exception the source does not state.

**Status.** `adopted`

**Where.** `phonology/conjunct.go:198`, `DissimilateGlides`;
`phonology/inventory.go:201`, `VowelFormAfterGlide`.

### §1.3.1 — the grave accent is read but not written

**Source.** §1.3.1 puts a grave on the **-i-** of a **-Cìa-** conjunct
to mark it as a syllable of its own rather than the glide of a **Cy+V**
sequence, and says the same "may similarly be used" on **-u-**. Its
examples are *karésìa* against *karesya*, *vélkìo* against *velkyo*,
and *ehùá*.

The rule as stated is mandatory for **-i-** and optional for **-u-**,
and Quijada never applies it: 100 words in the published corpus have
the exact **-Ci+V-** shape it describes and none carries the mark. The
grammar document's eight uses are §1.3.1's own two demonstrations, the
one **-u-** example, and five §7 foreign place-names.

**Decision.** Fold the grave away on input, so the grammar's own
examples parse. Do not write it back. Emitting it would produce forms
unlike anything attested; not emitting it contradicts §1.3.1 as
written, and that is a genuine open question rather than a settled
reading.

**Status.** `adopted`, with the output half unresolved.

**Where.** `phonology/normalize.go`, the `variants` replacer;
`phonology/stress_test.go:187`, the skipped
`TestApply_GraveOnUnstressedI`. Indexed in `BUGS.md` as an open
question about our output rather than about the language.

### §4.6.5 — a referential used as a lone Type-3 affix is glossed (refs/degree)

**Source.** §4.6.5 makes a lone Type-3 V_X C_S affix whose C_S is a
referential consonant read as a personal-reference shortcut rather than
a regular affix. Its trigger is "a lone Type-3 V_X C_S affix without
any adjacent Type-1 or Type-2 affix for it to apply to". §3.5 says a
Type-3 affix applies "to previous C_S V_X / V_X C_S affix only (or the
following affix if it is the first in the slot)", which means a Type-3
alone in Slot VII does have something to apply to when Slot V is
filled.

**Decision.** Resolve the trigger per slot, and gloss the referential
reading as `(refs/degree)` so it is visible as a referential rather
than silently rendered like any other affix. The notation is ours;
Quijada gives none.

**Status.** `adopted`

**Where.** `gloss/gloss.go:301`, `affixes`.

---

# Proposals

*Amendments we think the language wants. Not in force.*

None written up yet. The candidates are the three places no reading
can rescue, because the source leaves a construct with no expressible
form at all:

- **§3.6.1**, whose gemination rules reach only some of the 3840 Ca
  forms, so the rest cannot mark where Slot V ends.
- **§3.8.1.2**, where a C_N moved into the Slot VI position meets no
  gemination rule either, with the same consequence.
- **§3.6**, whose **bn**-substitution names one configuration on its
  input side and a different one in its intermediate, stranding
  whichever is left out.

Each needs an amendment rather than a reading, and each lands here as a
proposal once drafted.
