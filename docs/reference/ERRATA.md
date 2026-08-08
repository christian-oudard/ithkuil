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

### §3.6.1 — the nine gemination rules are a default plus exceptions

**Source.** §3.6.1 marks the end of Slot V by geminating the Slot VI
C_A form, and gives nine numbered rules for how. Taken as a dispatch
table that every C_A must match, they reach 3725 of the 3840 forms and
115 match nothing: rule 4 names seven of the nine sibilants, rule 6
five of the eleven fricatives, rule 7 six stop pairs and misses **kb**,
**pg**, **tb** and **tg**, rule 8 leaves **pň** and **tň**, and rule 3
reaches only forms beginning stop + liquid, leaving the 99 of shape
stop + stop + liquid.

**Decision.** Read them as a default plus exceptions. §3.6.1 says the
boundary is shown "by gemination of the C_A form", and geminating a
cluster means doubling its initial consonant; the numbered rules say
where that does not hold. Four of the nine restate the default and five
are genuine exceptions, and the shape of those five is a phonetic fact
rather than an accident: you cannot geminate mid-cluster except on a
fricative. Rules 4 and 6 are exactly the cases that double a medial
consonant and both double a fricative (**ksst**, **pff** are sayable);
rules 7 and 8 substitute rather than double because their inputs end in
a stop, and *akbbla* is not sayable where *akkbla* is.

Read this way the whole space comes out. All 3840 forms geminate, to
3840 distinct clusters, none of them equal to a bare C_A, and every one
of them survives being written into a word beside a Slot V affix,
pronounced, and read back to the grammar it was built from.

This repository made the dispatch-table mistake twice, and filed the
115 as a hole in the language both times, which is why the reading is
recorded rather than left in the code.

**Status.** `adopted`

**Where.** `allomorph/geminate.go`, `GeminateCa`, whose comment
tabulates which rules are default and which exception.
`allomorph/geminate_test.go` holds the cluster claims —
`TestGeminate_EveryFormIsCoveredAndDistinct` — and
`roman/ca_gemination_test.go` the whole-word ones, over all 3840 forms
with one and with two Slot V affixes.

### §3.8.1.2 — a moved C_N yields to a Slot V affix

**Source.** §3.8.1.2 lets a Pattern-1 Mood/Case-Scope C_N take the Slot
VI C_A position when C_A is the default -**l**-, putting **hl**, **hr**,
**hm**, **hn** or **hň** there. No §3.6.1 rule fires on an h-initial
cluster — **h** is not a stop, not a sibilant, not on rule 5's list of
non-sibilant fricatives, not a nasal or liquid, and none of the five is
a single consonant — so a word taking the shortcut has no way to mark
where Slot V ended. §3.6.2's glottal fallback does not reach it either,
being scoped to a C_A elided by the Slot I shortcut, and a formative
with that shortcut has no Slot VI C_A for §3.8.1.2 to replace.

**Decision.** The shortcut is unavailable when Slot V is filled. It is
optional throughout — §3.8.1.2 says such a C_N "may" take the position
— and taking it is what would cost the marker, so the case where it
cannot be paid for is the case where it is not offered. The long form
is always available: the default C_A geminated to -**ll**- with the C_N
back in its own slot, one syllable longer and carrying everything.

So there is no construct with no expressible form here, and the earlier
reading that found one had assumed the shortcut was obligatory once
legal. Nothing in the section says so, and the renderer already treated
every other optional shortcut the same way, by cost.

**Status.** `adopted`

**Where.** `slots/grammar.go`, `maybeMoveCnToCa`, whose first condition
is this rule; `roman/ca_gemination_test.go`,
`TestMovedCn_YieldsToASlotVAffix`.

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
### §§9-11 — the lexicon is not bound by the conjunct tables

**Source.** §§9, 10 and 11 of the phonotactics document enumerate the
tri-, tetra- and penta-consonantal conjuncts that "can be roots", as
products of per-position consonant sets. The community lexicon does not
stay inside them:

| table | product rows | roots of that length | outside |
|---|---|---|---|
| §9 tri | 129 | 2138 | 133 (6.2%) |
| §10 tetra | 357 | 2467 | 236 (9.6%) |
| §11 penta | 140 | 676 | 89 (13.2%) |

These are not exotic entries. `vskw`, `vsky`, `vskl` and the rest of
that block are ordinary vocabulary, and `lzbḑ` and `lzbv` sit in a
series whose other members §10 does permit.

**Decision.** Follow the lexicon. The tables describe what §§1-7 allow
rather than legislating over the vocabulary, and the same documents
disagree elsewhere in the same direction — see §4.7, where the grammar
publishes adjuncts the phonotactics forbids. A parser that rejected a
tenth of the lexicon would be wrong about the language as it is used.

It has a practical consequence for the entries below. Where we move a
root off a collided C_R we take a §10-permitted slot if one is free,
and say so where none is. For `-rţnw-` none was: §10's row for that
shape is `rř` + any + `n` + `wy`, so `rţn` admits only `w` and `y`,
and both are taken by the series that keeps the slot.

**Status.** `adopted`

**Where.** Nothing enforces the tables, which is the point. `phonology`
implements §§1-7, and `TestSection8GridMatchesRules` checks §8 against
them; §§9-11 are transcribed but not consulted.

---

# Lexicon

*Where the community spreadsheet, not Quijada, is the source.*

The roots and affixes come from the Collaborative Ithkuil IV
spreadsheet rather than from any of Quijada's documents, so these
entries are keyed by the C_R at issue instead of by a section. They are
held in `data/lexicon_overrides.json` and applied by `sync_lexicon.py`
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

**Where.** `data/lexicon_overrides.json`.

### -rţnw- — vitaceae 2 and rosoideae 7

**Source.** Rows 5118 and 5484, far apart. Two series overlap by one
slot. Rosoideae is a complete ten-member run —
`rţm rţmw rţmy rţml rţmr rţmř rţnw rţny rţň rţňw` — with no gaps.
Vitaceae is two entries, `rţn` and `rţnw`, whose second lands on
rosoideae's seventh.

**Decision.** Rosoideae 7 keeps `rţnw`; vitaceae 2 takes **`rţnl`**,
the next free slot in the series after `rţnw` and `rţny`. `rţnl` is
outside §10, which admits only `rţnw` and `rţny` for this shape and
has nothing free; see §§9-11 above. A ten-member
run with no gaps has the stronger claim than a two-member one that
overlaps it. The displacement is determined; the destination is chosen.

**Status.** `implemented`

**Where.** `data/lexicon_overrides.json`.

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

**Where.** `data/lexicon_overrides.json`.

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

**Where.** `data/lexicon_overrides.json`.

### -ḑg- — MDI and S07

**Source.** The affix sheet gives `ḑg` to both **MDI** Modification and
**S07** Position/state intertwined. `gḑ`, its transposition, is claimed
by no affix at all.

**Decision.** S07 keeps `ḑg`; MDI takes **`gḑ`**. A free slot that is
the exact transposition of the collided one, next to an entry that
needs a slot, is a typed-backwards pair rather than a coincidence.

This had already been fixed by hand in `data.json` and the fix did not
survive: the sheet is the source for affixes, the sync overwrote it,
and the store then refused the duplicate key. It is an override now, so
it survives.

**Status.** `implemented`

**Where.** `data/lexicon_overrides.json`.

---

# Proposals

*Amendments we think the language wants. Not in force.*

None. Three candidates stood here, each described as a place no
reading could rescue because the source left a construct with no
expressible form at all. All three turned out otherwise, and what they
needed was a reading or a correction rather than an amendment:

- **§3.6** was a correction, and is one now: see the entry above.
- **§3.6.1** is a reading, below. Its rules are a default plus
  exceptions, not a dispatch table, and read that way they reach every
  form.
- **§3.8.1.2** is the same reading one slot over, also below. The
  construct is not unwritable; the optional shortcut that would make it
  so is simply not available.

The lesson is worth keeping, since the list was wrong three times for
one reason. Each entry read a table as exhaustive and concluded the
language had a hole where the table ran out. Quijada writes rules that
say where a general case does not hold, and a rule that names no case
is not thereby a rule that forbids it.

### §6.2.2 — *Étkwö'e* is missing the Slot I marker its own gloss needs

**Source.** The §6.2.2 example sentence opens with **Étkwö'e**, glossed
on the site as `stem2/prc-FRAMED-'attend.scheduled.event'-PCR`: a stem,
a root and a case, with no Function/Specification/Context and no C_A.

Its three conjuncts are **é**, **tkw** and **ö'e**, and **ö'e** is the
whole of PCR Postcursive. Slots IV and VI are therefore both empty,
which is the §3.2 Slot IV/VI **a**+C_A shortcut and nothing else.
§3.1.5 and the two Slot II tables are explicit that the shortcut is
*shown* by a Slot I C_C of **w**- or **y**-, and that a Slot I of zero,
**h**- or **hw**- means the formative does not contain one. This word
has no Slot I at all, so as printed it needs a Slot IV and a Slot VI
and has neither.

The convention is not otherwise in doubt, and the sentence settles it
against itself. Six words later it has **wuttíhia**, glossed `[default
CA]-stem3/prc-FRAMED-'introduce'-RCP-APL`: the same construction —
a FRAMED verbal carrying the default-C_A shortcut under antepenultimate
stress — written with its **w**-. Across the whole corpus 177 words
carry a Slot I glide and none drops it; **we**-, **wa**-, **wu**-,
**wo**-, **ye**-, **ya**- and **yu**- are all attested, so no vowel
conditions it away, and *wétkwö'e* is phonotactically legal as it
stands. The only elision the sources give a word-initial **w**- is
§5.8.8's, where the sentence-juncture **ç**- merges with it to give
**çw**-, which adds a letter rather than removing one.

The glide is not decoration that could be left off. The Slot II table
is indexed by both: the V_V vowel gives the Stem, Version and series,
and **w**- against **y**- picks which C_A that series stands for —
[default] against PRX, G against RPV, N against A, G/RPV against
PRX/RPV. So *wétkwö'e* and *yétkwö'e* are both well-formed and differ
only in C_A, and with neither glide there is nothing to say which C_A
was elided.

**Decision.** Read it as a slip for **Wétkwö'e**, which parses to
exactly the published gloss, stress included — antepenultimate over
three syllables is the FRAMED relation. Do not repair the corpus:
`corpus/examples.txt` transcribes the site, and the site says
*Étkwö'e*. The word stays in the drift guard as one we cannot read,
now with a reason rather than as an unexplained failure.

**Status.** `adopted`

**Where.** `roman/corpus_test.go:38`, the `corpusUnclassified` entry.
