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

### §3.6 — the bn-substitution family is one rule short

**Source.** §3.6's allomorphic substitutions include two escapes for
clusters the general rules leave unsayable:

    fbm → (fv) → vw
    ţbn → (tḑ) → ḑy

Both are printed in blue, and the v1.3 change list says "Two of the
allomorphic substitutions for C_A have been modified", so these two are
that revision.

Sweeping every C_A whose raw composition ends in **bm** or **bn** — the
inputs the general `[C]bm → [C]v` and `[C]bn → [C]ḑ` rules act on —
exactly three shapes come out unsayable:

| raw | becomes | barred by |
|---|---|---|
| `fbm` | `fv` | §2.5, homologous voicing mismatch |
| `ţbn` | `ţḑ` | §2.5, homologous voicing mismatch |
| `tbn` | `tḑ` | §2.2, dental stop plus interdental |

The revision covers the two §2.5 cases and is complete for them. `tbn`
is barred by a different rule and has no escape at all.

Two things are therefore wrong. The printed intermediate `(tḑ)` cannot
arise from `ţbn`: `[C]bn → [C]ḑ` rewrites only the **bn**, so `ţbn`
yields `ţḑ`. And the rule beside it is the exact parallel — both voice
the fricative and open the nasal into an approximant, f→v with m→w and
ţ→ḑ with n→y — which makes `(ţḑ)` the reading and `(tḑ)` the slip.

**Decision.** Two changes to §3.6's substitution list. Correct the
intermediate of the rule that is there, and add the rule that is
missing:

| | printed | ours |
|---|---|---|
| unchanged | `fbm → (fv) → vw` | `fbm → (fv) → vw` |
| corrected | `ţbn → (tḑ) → ḑy` | `ţbn → (ţḑ) → ḑy` |
| added | — | `tbn → (tḑ) → ḑw` |

`ḑw` keeps the interdental the composition was already heading for and
varies only the approximant, so `ḑy` and `ḑw` are a minimal pair the
way `vw` sits beside them. Voicing the stop to `dḑ` would be tidier
still but is barred by §2.2 in its turn.

All eight resulting forms — both escapes across all four Affiliations —
are phonotactically legal, unused by any other C_A, and have legal
unused geminates reachable by §3.6.1 rule 5. With the family complete,
all 3840 C_A values compose to a legal cluster and none collides.

**The sentence it costs.** `-lḑ-` is "tree", and this C_A is the idea
of a set of trees dying off, taken as a representative case. MSS makes
the members alike, MDS unlike — a monoculture against a mixed wood,
which is a distinction a forester draws.

Before, with `tbn` routed through `ḑy` so that both spell the same:

    Čalörá alḑaḑya alḑaḑyue.
    'eq'-NEG/6-ASR   lḑ-MSS.A.DPL.RPV   lḑ-MSS.A.DPL.RPV-CMP

Both nouns decode as MSS, so the sentence reads "a dying-off of like
trees is not the same thing as a dying-off of like trees" and refutes
itself. The one distinction it exists to draw is the one that is gone.

After, with `tbn → ḑw`:

    Čalörá alḑaḑwa alḑaḑyue.
    'eq'-NEG/6-ASR   lḑ-MSS.A.DPL.RPV   lḑ-MDS.A.DPL.RPV-CMP

    "A dying-off of like trees is not the same thing as a dying-off
     of unlike trees."

`alḑaḑwa` parses back to MSS and `alḑaḑyue` to MDS. Every word is built
from the lexicon and round-trips through the parser.

Three further arguments for treating `tbn` the same way, beyond
symmetry.
Articulation: the family is a sayability gradient, `fbm` unsayable →
`fv` hard → `vw` easy, and `tbn` awkward → `tḑ` hard → `ḑw` easy sits
in it. Epenthesis:
a speaker resolving `atbna` reaches for *atabana*, which is a legal
Ithkuil word of four syllables rather than two, and syllable count is
what stress is measured against, so the repair a speaker improvises
changes the grammar rather than just the sound. Cost: without a rule,
MSS/A/DPL/RPV has no form across all four Affiliations, and routing it
through `ḑy` instead would make *alḑaḑya* mean both a like and an
unlike set — so the sentence *Čalörá alḑaḑya alḑaḑyue*, "a dying-off of
like trees is not the same thing as a dying-off of unlike trees", would
refute itself.

**Status.** `implemented`

**Where.** `allomorph/substitutions.go:46`,
`secondPassSubstitutions`, pinned by `TestSubstitutions_BnFamily`.
`TestCa_EveryFormIsPronounceable` and
`TestCa_EveryGeminateIsPronounceable` sweep all 3840 values and their
geminates; both used to be excused by `UnresolvedCa`, which now returns
false for everything.

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

# Lexicon

*Where the community spreadsheet, not Quijada, is the source.*

The roots and affixes come from the Collaborative Ithkuil IV
spreadsheet rather than from any of Quijada's documents, so these
entries are keyed by the C_R at issue instead of by a section. They are
held in `data/root_overrides.json` and applied by `sync_lexicon.py`
after each fetch, so an upstream repair supersedes them automatically
and reverting one is a single line.

A C_R names one root, so four C_R values carrying two unrelated live
meanings each is a defect in the sheet. Three are clerical and one is a
real double-claim. Note the asymmetry of confidence: only `cfy` is
*recovered*, in the sense that the data determines it. The rest are
*chosen*, and a later upstream repair may well choose differently.

The biological sections assign a C_R by extending a stem with a fixed
suffix series, `w y l r ř f ţ ç m …`, one slot per taxon. Every
clerical collision is a break in that series, which is what makes them
diagnosable.

### -cfw- — magnoliaceae and myristicaceae

**Source.** Rows 5575 and 5576 of the sheet, adjacent, both `cfw`. The
`cf` run reads `cfw, _, cfl, cfr, cfř, cff`: the second slot of the
series is empty and `cfy` occurs nowhere among the 5891 roots.

**Decision.** Magnoliaceae keeps `cfw`; myristicaceae takes **`cfy`**.
Row 5575 sits where the series puts it and row 5576 failed to advance
the consonant, so this is a copy-down and the vacant slot is exactly
the one the series demands. Recovered, not chosen.

**Status.** `implemented`

**Where.** `data/root_overrides.json`.

### -rţnw- — vitaceae 2 and rosoideae 7

**Source.** Rows 5118 and 5484, far apart. Two series overlap by one
slot. Rosoideae is a complete ten-member run —
`rţm rţmw rţmy rţml rţmr rţmř rţnw rţny rţň rţňw` — with no gaps.
Vitaceae is two entries, `rţn` and `rţnw`, whose second lands on
rosoideae's seventh.

**Decision.** Rosoideae 7 keeps `rţnw`; vitaceae 2 takes **`rţnl`**,
the next free slot in the series after `rţnw` and `rţny`. A ten-member
run with no gaps has the stronger claim than a two-member one that
overlaps it. The displacement is determined; the destination is chosen.

**Status.** `implemented`

**Where.** `data/root_overrides.json`.

### -lzbḑ- — psychodomorph and tabanid fly

**Source.** Rows 4296 and 4297, adjacent. The `lzb` series holds nine
suffixes — `ḑ w y l r ř z ž v` — the same nine the neighbouring `lzg`
series uses, and it is full. The section then moves to the `lzk` stem,
which draws on a wider set including `f`, `m`, `ç` and `h`.

**Decision.** Psychodomorph keeps `lzbḑ`; tabanid fly takes **`lzbẓ`**.

Every suffix the `lzb` series uses is voiced — `ḑ w y l r ř z ž v` — and
the lexicon bears the constraint out: `bf` occurs in 3 roots out of
5891, against 45 for `bv` and 138 for `bz`. So a voiceless continuation
is what the stem does not take, and `ẓ` continues the `z`/`ž` sibilants
already in the series. An earlier draft of this entry said `lzbf`,
picked from the wider set the neighbouring `lzk` stem draws on, which
was wrong for exactly that reason.

This is the weakest of the four and it is worth saying why. The
taxonomy argues the other way: Psychodomorpha is a nematoceran, which
puts it with the `lzg` run that ends at 4295 in "other nematoceran
fly", while Tabanidae sits inside Tabanomorpha, which is `lzbw`
immediately below. So the row that looks misplaced is the first one,
not the second. Following that would move psychodomorph into the `lzg`
series instead, which is also full. We keep the row order because it is
evidence we can check and the taxonomy is not, but a maintainer who
knows the sheet's intent should overrule this.

**Status.** `implemented`

**Where.** `data/root_overrides.json`.

### -nļt- — groin undergarment and cicadomorphic bug

**Source.** Rows 1417 and 4117, three thousand rows apart, in unrelated
domains. Both have a structural claim. The clothing block uses `nļt`
as one member of a set varying the final consonant — `nļt`, `nļp`,
`nļm`. The insect block uses `nļt` as a **stem** and extends it eight
times: `nļtw`, `nļty`, `nļtl` and five more.

This is the one real double-claim of the four; no series is broken and
nothing is recoverable.

**Decision.** The cicadomorphic bug keeps `nļt`; the undergarment takes
**`nļx`**. Purely a cost argument: moving the insect root drags eight
dependants with it, moving the clothing root moves one, because `nļp`
and `nļm` stand on their own. `nļx` rather than the nearer `nļţ`,
which differs from `nļt` by a cedilla and would be read wrong.

Chosen, not recovered, and on the weakest grounds of the four. Whoever
maintains the sheet should decide this rather than us.

**Status.** `implemented`

**Where.** `data/root_overrides.json`.

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
