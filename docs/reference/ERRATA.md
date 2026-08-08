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
- **Noted.** The source prints something wrong and the reference
  document reproduces it anyway. The decision is to leave it alone, and
  a reader needs to know it was a decision.
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

### §2.14 — two prohibitions moved from the prose into the tables

**Source.** Two constraints are enforced by every table in the
phonotactics document and stated by none of its rules. They were prose
rules in the two editions before v0.5.4:

> **v0.3 / v0.4 §2.6.** The voiced interdental fricative -**ḑ**- cannot
> be followed by any of the four sibilant fricatives.
>
> **v0.3 / v0.4 §2.15.** The nasal **n**- cannot be followed by the
> labial stops -**p** and -**b** ... Nor can -**n**- be followed by
> -**ň**-.

Between v0.4 (June 2019) and v0.5.0 (January 2021) §2 was renumbered
from twenty-three rules to twenty-two. The **ḑ** rule went entirely;
the **n** rule survives as v0.5.4's **2.14** word for word, less its
final sentence.

The constraints did not go with them. §8's matrix marks all five
impermissible — the **ḑ** row's blue squares are ţ, s, š, z, ž and the
**n** row's include ň — and the printed row totals confirm the count
without reading the colour, 25 of 30 for **ḑ** and 23 of 30 for **n**.
§3.6's substitution table says it from the other side, carrying
**ngn → ňn** as a named exception to **[C]gn → [C]ň** whose only effect
is to keep a derivation off **nň**.

**Decision.** Enforce all five, and cite **§8** for them rather than a
§2 number, because §8 is where the current document states them.

Our transcription previously carried them as a rule "**2.23**". That
number is not free: v0.3 and v0.4 both end at 2.23, and their 2.23 is
the **w**/**y** rule that survives as today's **2.22**. So the
transcription merged two rules under a number belonging to a third.
It has been removed.

A neighbouring "**2.24**" barring **çç** and **ļļ** has been removed
too, and that one was invented outright. No edition prohibits them,
v0.3 and v0.4 list **ļļ** in their tables as permitted, and Quijada's
own material behaves throughout as though they are: §3.6.1 rule 4
geminates a sibilant "in any position" with **çkl → ççkl** as its
worked example, rule 6 gives **tçkl → tççkl**, and the bias-adjunct
table holds **pļļ** (CMD) and **kçç** (EXA). Identical provenance to
the rule above and the opposite answer, which is why the two should
not have been decided together.

The lexicon corroborates both halves: across 5,946 roots and 528
affixes, **ḑ** heads 152 clusters over at least eight following
consonants and never a sibilant, and **n** heads 748 and freely takes
s, z, š and ž but never **ň**.

**Status.** `adopted`

**Where.** `phonology/phonotactics.go:194`, the two checks citing "8".

---

# Readings

*Where the source cannot be implemented as written, and we chose.*

### §2.3 — a concatenation chain has no parsing adjunct

**Source.** §2.3 makes pitch accent "the means by which word boundaries
may be parsed", and ¶5 supplies a fallback for when that channel is
unavailable:

> In unusual situations (e.g., singing a song) when pitch-accent is
> unavailable or undesirable as a means of parsing word boundaries and
> the placement of pauses between words is unrealistic, then a special
> parsing adjunct of the form **'V'** may be placed before any word to
> be parsed, where **'V'** represents a single vowel between two
> glottal stops, the particular vowel indicating the syllabic stress of
> the following word.

One adjunct declares one stress. A §3.1.7 concatenation chain is
written as a single hyphen-joined word, but each link carries its own
stress and its own word-initial and word-final positions, so a chain
has as many stresses as it has links: *hakšiţé-alcialu'a* bears
ultimate stress on the first link and penultimate on the second.

So an adjunct cannot declare a chain's stress, and the section gives no
way out. "Any word to be parsed" would have to mean a chain *link*
rather than the whole chain, which would put an adjunct inside the
hyphenated word — *'e' hakšiţe-'o' alcialu'a* — and nothing sanctions
that. The alternatives are no better: that chains cannot be sung, or
that a chain takes one adjunct naming only the parent's stress and the
listener recovers the rest from the Cc markers.

**Decision.** Refuse rather than choose. Rendering a chain without
stress diacritics returns an error naming this entry. Everything else
round-trips, 554 corpus words among them.

Where the source leaves a construct undetermined we would rather fail
at the point of use than pick a spelling and have it read back later as
Quijada's. This is the one construct in the language for which we
decline to emit anything at all.

**Status.** `adopted`

**Where.** `roman/stressless.go:42`.

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

The suppletive adjuncts of §4.5 are the same collision in miniature and
easier to see. They are **hl**-, **hm**-, **hn**- and **hň**-, and
§4.5.4 prints worked examples of the last: *hňa, hňei, hňo, hňe'e,
hňa'u, hňi'a*. Phonotactics §3.2.7 permits word-initial **h**- to be
followed by "-l or -r, the nasals -m or -n, or by -w", and not -**ň**.
§3.8.1.2's **hňw**, a C_N that can move into Slot VI, is unlicensed
twice over: §3.3.5 limits word-initial triconsonantal **h**- conjuncts
to hlw, hrw, hmw, hnw, hmy, hny and four geminates. The sequence
**hň** does not occur anywhere in the phonotactics document, in any
position.

§4.4's registers are the story from the vowel side: their adjuncts are
a single vowel-form after **h**-, and every one is fine, but only
because the table stops where it does.

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

### §4.6.1 — Rule 3 governs a referential slot only when the word ends there

**Source.** §4.6.1 lists eleven examples and adds a parenthetical:
"Note that the last three of these examples illustrate that Sec. 1.7,
Rule 3, applies to Slot 2 V_C1 and Slot 3 V_C2 for Cases 37 through
52." The three are *sme'e*, *ka'u* and *fo'we'is*.

The first two bear it out. Both have no Slot 3, so V_C1 ends the word:
**sme'e** reduplicates *e* around the glottal and **ka'u** takes it
intervocalically in the diphthong *au*, which is Rule 3 in each of its
two shapes.

The third does not. **fo'we'is** writes V_C1 as **o'**, the glottal
after the whole vowel-form, which is Rule 1. Rule 3 would give
**fo'owe'is**. Its V_C2 *is* Rule 3, as **e'i** for *ei*.

**Decision.** Follow §1.7 and the printed example over the
parenthetical. Rule 1 is §1.7's default and Rule 3 overrides it only
where Rule 1 would be phonotactically impermissible or would leave the
glottal word-final. V_C1 in *fo'we'is* has the Slot 3 **w** behind it
and so is neither. Read that way the parenthetical is loose rather than
wrong: it names §1.7 as the reason glottal stops appear in those slots
at all, not which of its rules applies.

So a V_C1 with a Slot 3 behind it is written by Rule 1, and a
word-final one by Rule 3. The alternative is to take the sentence at
its word and call the printed example a slip, which would make three of
the section's own eleven examples disagree with the sentence describing
them.

**Status.** `adopted`

**Where.** `roman/referential.go:33`, which offers the Rule 1 spelling
where one exists; `roman/parse_referential.go:269`,
`absorbRule1Glottals`, which reads it back.

### §3.9.3.2 — VERIFICATIVE is abbreviated VER, not VRF

**Source.** The morphology document cannot make up its mind. §3.9.3.1's
table and the illocution/validation matrix both say **VRF**; §3.9.3.2's
V_K list says **VER**; and the v1.3.2 version history says "The
3-letter abbreviation for VERIFICATIVE Illocution has beeb changed to
VER." So the rename was made, announced, and then applied to one of the
three places.

The collision is presumably what it is for. The affix document gives
the same three letters to -**ňç** "Verifiability of Info &
Trustworthiness of its Source", and reprints the illocution matrix with
VRF in it, so both senses appear on one page.

**Decision.** Write **VER**. This is a choice between two things the
source says, not a departure from it: the version history instructs the
rename and one of the three tables has already had it applied.

The reference document prints each of the three places as the source
does, VRF twice and VER once. Our own transcription had VRF in all
three until this was worked, having missed the one VER.

A gloss is a sequence of abbreviations with no per-token type, so the
two senses could not otherwise be told apart in one. Even now they are
distinguished only by the degree an affix carries — `VRF/3` is the
affix and a bare `VRF` the illocution — which is why writing the
illocution VER is worth the inconsistency with two of the three tables.
The illocution list is otherwise collision-free.

**Status.** `adopted`

**Where.** `grammar/vk.go:43`, `Verificative.Tag`.

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

The split at two consonants is the first thing worth noticing. §8 is a
grid derived from the pair rules of §§2-7, so a bi-consonantal root our
validator accepts is one §8 accepts, and all 670 short roots pass. §§9,
10 and 11 are independent tables that no rule generates, and the whole
of the disagreement is there.

The departures are systematic rather than scattered, which is the
argument that the tables are incomplete rather than the lexicon
careless. `-pf-` accounts for fourteen tri-consonantal cases on its
own: §9 gives medial **p** a third consonant **f** only after the
initials `sšç`, and the spreadsheet also writes `ţpf`, `žpf`, `ḑpf`,
`ẓpf`, `čpf`, `jpf`, `kpf`, `gpf`, `dpf`, `xpf`, `zpf`, `ňpf`, `ļpf`
and `cpf`, plus `pfc` and `pfč` with the pair leading.
`Cml`/`Cmr`/`Cmř` is a second family of the same kind: `cml`, `sml`,
`vml`, `zml`, `ẓml` and their `r` and `ř` partners. Two affixes depart
as well, `xḑr` and `čḑr`, sharing a final `-ḑr` that §9 gives no
initial at all. §§8-11 below is a further reason to hold the tables
lightly: none of the four agrees with its own arithmetic.

What is not in question is that the check should exist.
`phonology.RootConjunctLegal` implements §§9-11 and **fbm** is the case
that motivated it, a conjunct both of whose pairs §8 permits, that
appears in no §9 row, and that a speaker asked to say reported
impossible. It is not consulted anywhere, because enforcing it as
things stand would refuse 8% of the vocabulary.

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

# Noted

*Where the source is wrong and we transcribe it as printed anyway.*

Reproducing a defect is a decision, and one a reader will otherwise
mistake for a transcription error and try to repair. Nothing in this
section changes our output.

### §3.9.3.1 — HORTATIVE and POTENTIATIVE are named for categories they are not

**Source.** §3.9.3.1 defines the two illocutions as a matched pair,
separated by whether the wish can come true:

- **POT** POTENTIATIVE: "a statement of wishing, hoping, or other
  unreal(ized) provenance"
- **HOR** HORTATIVE: "a counterfactual statement indicating a desired
  but impossible state of affairs that cannot be realized (equivalent
  to English hortative constructions such as *If only..., Were that...,
  If only it were so that...*)"

Neither name carries that meaning outside this document. A hortative
exhorts: Allen & Greenough §439, "the Hortatory Subjunctive is used in
the present tense to express an exhortation or a command", and the
English instance of it is *let's*. A potential expresses probability,
that the speaker holds the event likely. What both definitions describe
is the **optative**, the mood of wishing, and the split between them is
the one Allen & Greenough §441 draws inside the optative subjunctive by
tense: "the present tense denotes the wish as possible, the Imperfect
as unaccomplished in present time". The tradition has one name for the
category and a tense contrast for the division; V4 needs two labels and
took two wrong ones.

The naming is deliberate rather than a slip, and it predates the
category. No HORTATIVE illocution exists in the 2020 drafts. From
v0.12.0 through v0.17.2 each carries the same sentence: "Hortatives
('if only.../were it so that...') are expressable by the combination of
PERFORMATIVE Illocution + EXECUTIVE Expectation + COUNTERFACTUAL Mood."
The word was already the author's name for the English construction
while it was still a three-part periphrasis, and when Illocution was
restructured for the 1.x releases the new value inherited it.

**Decision.** Keep both names. Nothing here is ambiguous and nothing in
the code is wrong: HOR is counterfactual, and exhortation to act is
DIR, whose own definition covers "an imperative command to another
party to do/be something". Renaming a published value to match outside
usage would cost every existing gloss and text its readability, and buy
correctness in a vocabulary the language does not otherwise borrow.

What it costs is the reader who knows the term, who reaches for HOR to
say "let's" and gets "if only". Anything that teaches the illocutions
should gloss these two rather than list their names, which is why
`data.json` carries `guidance` on both.

**Status.** `adopted`

**Where.** `grammar/vk.go:43`, in the surrounding illocution list; the
`guidance` fields for HOR and POT in `data/data.json`.

### §§8-11 — none of the four conjunct tables agrees with its own arithmetic

**Source.** §§8-11 tabulate the permissible consonant conjuncts. Each
is derived from the rules in §§1-7, and each carries per-row totals and
a grand total. In all four the rows, the row totals and the grand total
are three different answers.

| | rows enumerate | printed row totals | stated total | rows disagreeing |
|---|---|---|---|---|
| §8 bi-consonantal | 684 | 679 | 679 | 5 of 27 |
| §9 tri-consonantal | 5021 | 5183 | 5183 | 4 of 129 |
| §10 tetra-consonantal | 15106 | 14974 | 15034 | 7 of 357 |
| §11 penta-consonantal | 12089 | 12011 | 12271 | 5 of 140 |

§8 is a grid of coloured cells, and its five short rows — ç, c, č, ẓ
and j — each hold one more permissible cell than the total printed
beside them. Nothing marks which cell is meant to be excluded.

§§9-11 are product-of-sets tables: a row gives a permitted consonant
set per position, and its total should be the product of the set sizes.
485 of the 497 rows are exactly that. The twelve that are not were each
checked against the rendered page, so the sets are as printed and the
arithmetic is the source's:

- **A dropped factor, twice.** §9's `rř` + **ḑ** row prints 24, which
  is its third-consonant count alone; §10's `rř` + 21-consonant + **n**
  + `wy` row prints 42, which is 21 x 2. Both omit the leading `r ř`
  from the product, and in both cases the neighbouring rows with a
  single-consonant first column are correct.
- **A digit typo.** §9's `c` + **c** row prints **223** where its
  contents give 23. On its own that accounts for 200 of §9's 162-form
  gap; correct it and the printed totals sum to 4983.
- **Parallel blocks that disagree.** §11 tabulates **z** and **ž** in
  blocks with identical set structure. The `mn`/`wy` row prints 45 in
  the z block and 36 in the ž block; 36 is the product.
- The other eight are off by amounts with no evident cause, the largest
  being §10's `rřl` + `bgv` + **z** row, printed 64 where its sets give
  144.

**Decision.** Transcribe the totals as printed and derive nothing from
them. The rows say which conjuncts exist and the totals are commentary
on the rows, so nothing about the language turns on this. Repairing
them in place would be inventing figures Quijada never published.

It does mean no count he prints for these tables can be quoted as
authoritative, including the widely-cited **679 bi-consonantal
conjuncts**, which is one of the five-row disagreements rather than a
figure the grid supports.

**Status.** `adopted`

**Where.** `phonology/section8_test.go:34` regenerates §8 from §§1-7
and finds 684, not 679. Nothing consults the printed totals.

### §4.5.3 — the naming-adjunct examples hold a form from the adjunct above

**Source.** Each of §4.5's four suppletive adjuncts prints six example
words built on its own C_P: `hla, hlei, hloa, hle'e, hla'u, hli'a` for
CAR on **hl**, and the same six vowel-forms on **hm** for QUO, **hn**
for NAM, **hň** for PHR. Two entries break the pattern:

- NAM's second example is **hmei**, which is QUO's. On the pattern it
  should be `hnei`, and every other NAM example is `hn`-initial.
- PHR's third example is **hňo** where the other three rows have the
  Case-3 form `-oa`. It should be `hňoa`.

**Decision.** Transcribe both as printed. Nothing rests on them: the
C_P values are stated separately in the same table and are not in
doubt, and these are single-word slips in an otherwise mechanical list.

Recorded because a reader building a test set from these six-word rows
would otherwise get one adjunct's form filed under another's, and
because the two look exactly like the transcription errors this
document has spent so long removing.

**Status.** `adopted`

**Where.** `morphology.md` §4.5.3, the four example rows.

### -ï- — the phonotactics vowel chart has a tenth vowel

**Source.** The Phonemic Inventory of the phonotactics document prints
a vowel chart with **ï** in the high central unrounded cell, beside i,
ü and u. The grammar document's §1.1 chart has the same three and no
fourth; the vowel-form table of §1.6 is built from nine vowels, not
ten; and **ï** appears nowhere else in the phonotactics document, in
any rule, in any conjunct table, or in any root or affix in the
lexicon.

**Decision.** Transcribe it as printed, in the cell it occupies, and
implement nine vowels. The cell is the whole of the claim. An
implementation that added a tenth vowel on this evidence would have no
vowel-form to put it in, no rule mentioning it and no word using it.

**Status.** `adopted`

**Where.** `phonology/inventory.go`, which holds nine vowels;
`phonotactics.md`, the Vowels table.

### -žţ- — PIC names two affixes, and only one reached the spreadsheet

**Source.** The chemistry section of the affix document gives four
Polyatomic Ion affixes on one line — `-cţ` PIA, `-ẓţ` PIB, `-čţ` PIC,
`-jţ` PID — and then, fourteen lines later, a fifth: **`-žţ` PIC
Additional Polyatomic Ionic Configurations**, whose nine degrees are
the oxyanion series (-ate, -ite, hypo-...-ite, per-...-ate, -ide,
bi-...-ate, dihydrogen...-ate, di-...-ate, di...-ide).

So PIC abbreviates two different affixes, with different C_S forms and
unrelated meanings. The community spreadsheet carries only `-čţ`, and
`data.json` and `affixes_reference.md` follow it, so `-žţ` is absent
from our data entirely; the cluster appears there only as a root.

**Decision.** Leave it absent. The affix rows come from
`tools/sync_lexicon.py` mirroring the spreadsheet, so a hand-added row
would be overwritten on the next sync, and the gap belongs upstream.
The duplicate abbreviation belongs to Quijada either way, and is not
ours to resolve.

Adding it as a local override, the way the C_R collisions below are
handled, would be a different thing: those move a root the sheet
already has, while this would invent a row the sheet has never carried.

**Status.** `adopted`

**Where.** `data/lexicon_overrides.json` does *not* carry it, which is
the decision.

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
