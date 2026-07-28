# Issues in the V4 Source Material

Defects in the published grammar, the affix tables and the community
lexicon — not in this repository's code. Defects in our own handling
are tracked as skipped tests beside the code they concern, not in a
list; anything we cannot implement because the sources disagree gets an
entry here and a test there.

## Provenance

Four layers sit between Quijada and this repository, and a defect can
belong to any of them.

1. **Quijada's V4 design documents.** *Obtained and checked.*
   `New_Ithkuil_design_doc_v_1_3.pdf` from ithkuil.net is Grammar
   Design v1.3.2 (Feb 15 2023), the version our markdown transcribes.
   `ithkuil.place/4/archive/latest/` supplies the affix, phonotactics
   and script documents. Local copies live in
   `$XDG_DATA_HOME/ithkuil/reference/`, outside the repo.
2. **The Collaborative Ithkuil IV Roots and Affixes Spreadsheet**
   (Google Sheets `1JdaG1PaSQJRE2LpILvdzthbzz1k_a0VT86XSXouwGy8`),
   which `tools/sync_lexicon.py` mirrors. *Fetched live and checked.*
3. **`language_reference/*.md`** — our markdown transcriptions of layer 1.
4. **`data/data.json`, `data/*.tsv`** — layer 2 plus local supplements.

The reference documents carry the source and only the source,
including where the source is wrong: a defect is recorded here, not
corrected in place, so that the two can be compared. Provenance,
commentary and open questions belong in this file rather than inline
in the transcription.

### What each reference document is

| File | Source | Fidelity |
|---|---|---|
| `morphology.md` §§1-7 | New Ithkuil: Grammar Design, v1.3.2 (Feb. 15, 2023), 67 pp. | transcription |
| `morphology.md` §8 | Chapter 13 of the *2011* grammar, `ithkuil.net/newithkuil_13_numbers.htm` | condensed, in our words |
| `phonotactics.md` | Phonotactic Rules for the Ithkuil Successor Language, v0.5.4, 24 pp. | transcription |
| `affixes_reference.md` | `data/data.json`, reconciled against V_XC_S Affixes for New Ithkuil v1.1 | generated content |
| `CHANGELOG.md` | the version histories of the above | transcription |

One chapter of `morphology.md` has no layer-1 source at all.

**§8, the number system.** No V4 document covers numbers: the grammar
document ends at Chapter 7, and none of the four so much as mentions
the centesimal system. §8 is condensed from Chapter 13 of the 2011
grammar and renumbered 13 to 8; the chapter number and the subsection
numbers are both ours. Its twenty number roots all match the V4
lexicon exactly, which is the evidence that the system carries over
unchanged. Nothing else in it is corroborated by a V4 source.

A finding drawn from the wording of that chapter is a finding about
our own prose. G19 is the cautionary case.

The writing system is not covered here at all. It is a separate
document of Quijada's, not a chapter of the grammar, and belongs with
the script work rather than with the grammar; the material is on the
`writing` branch, bound for its own repository.

**Unaudited:** the cluster tables in `phonotactics.md` §§8-11 and the
Slot tables in `morphology.md` §3 have not been read against the PDFs
cell by cell.

### What checking against layer 1 changed

Ten findings have been withdrawn in whole or in part. Eight were
defects in **our markdown** rather than in Quijada — G3, G4, G14, G17,
G19, G27, G31, G32 — and one, A2, was a spreadsheet transposition.
They are kept below, marked WITHDRAWN, with what the source actually
says, because the transcription and the data still need fixing even
where the language does not.

The largest was G3. Quijada's Ca table binds its two alternate-form
rules with superscript footnote markers: Extension entries carry ¹
("if the Configuration of the word is UPX") and the RPV Perspective
entries carry ² ("when preceded by [C]t-, [C]k-, or [C]p-"). Our
markdown dropped the superscripts and rendered the footnotes as
free-standing sentences, which made the conditions look swapped. The
binding in the PDF is the one the 3840-value bijection test had already
identified as the only workable reading — the computation was right and
the source was never wrong.

Two entries changed shape rather than falling: A3 (SPT really does have
two C_S forms for one degree list) and A5 (the six blank
functional-group affixes are populated in Quijada, so the gap is
recoverable).

**Not yet re-checked against layer 1:** G1, G2, G5-G13, G15, G16, G18,
G20-G26, G28-G30, and the Lexicon and Corpus sections. These are
structural — missing rules, uncovered cases, tables that cannot be
functions — and
do not turn on single characters, so a transcription slip is unlikely
to explain them away. But G3 and G4 looked structural too. Treat them
as provisional until each is read against the PDF.

## Affix table
### A1. Three affixes' degree lists are shifted by one row

*Confirmed in the live spreadsheet (layer 2), not an artifact of our
mirror.* Rows 487-489 hold the wrong meanings:

| Cs | Abbrev | Description | Degrees in the spreadsheet | Degrees in the reference |
|----|--------|-------------|----------------------------|--------------------------|
| ẓd | MET | Metonymic Categories | *(blank)* | part for whole, producer for product, … |
| ẓḑ | GPJ | Functional Group J | part for whole, producer for product, … | thiocyanate, isothiocyanate, … |
| zf | ENS | Environmental Niche | thiocyanate, isothiocyanate, … | active at twilight/crepuscular, nocturnal, … |

Both sources agree on every Cs, abbreviation and description; only the
degree cells differ. The reference's assignment is the coherent one —
"part for whole" is metonymy, thiocyanate is a functional group, and
crepuscular/nocturnal is an environmental niche — so the spreadsheet
is what slipped.

The shape is what an "insert cells, shift down" over a three-row
selection produces: the first row is blanked, the first two rows'
contents move down one, and the third row's contents fall out of the
selection and are lost. ENS's nine meanings appear nowhere in the
spreadsheet; they survive only in the reference document.

Upstream, MET's Degree 1 is empty, GPJ's is "part for whole" and ENS's
is "thiocyanate, thocyanato-, -thiocyanate" — exactly as our mirror has
them. `data/data.json` is built from that sheet, so the shipped lexicon
glosses MET as empty, GPJ as metonymy and ENS as thiocyanate chemistry.
The reference document is the only place ENS's nine meanings survive.
**Fixed:** `tools/sync_lexicon.py` now carries a `SHIFTED_DEGREES`
override that restores all three from there on every sync, and
`data/data.json` has been corrected.

### A2. WITHDRAWN — the ḑg collision is a spreadsheet transposition

The live spreadsheet has `ḑg` twice, for MDI and S07, and it is the
only duplicated C_S among its 527 rows. Quijada's affix document does
not:

| | Quijada | Spreadsheet |
|---|---------|-------------|
| S07 Position/state intertwined in 3-D volume | **-ḑg** | ḑg |
| MDI Modification | **-gḑ** | ḑg |

MDI's cluster is `gḑ`, reversed upstream into `ḑg`, which manufactured
the clash. There is no collision in the language, and `gḑ` is free.

**Fixed** in `data/data.json`. It should also go upstream, where a
duplicate-key check would catch it.

### A3. SPT has two C_S forms and no rule for choosing between them

Quijada's affix document gives the entry as **-rw/-ry SPT Specified
Points in Calendrical Time**, with a single degree list: second(s) of
the minute, minute(s) of the hour, hour of the day, day of the week,
day of the month, week of the month, month of the year, year, century.
§6.0 of the grammar repeats the same pairing. The spreadsheet's two
identical SPT rows mirror that faithfully; they are not a duplication
error, as an earlier version of this entry supposed.

So the affix really does have two consonant forms for one meaning set,
and nothing anywhere says which to use.

What makes the silence odd is that the surrounding family uses the same
alternation to separate *different* affixes. Fourteen C_S forms differ
only in a final -w against -y, and in every other case the two are
distinct entries, -w the nearer member of a paired scale and -y the
farther:

| -w form | | -y form | |
|---------|---|---------|---|
| rkw | CYC Cyclic Recurrence (every second … every century) | rky | CYL Cyclic Recurrence **[Long-Term]** |
| rţw | ITE Iterations Per Time-Period | rţy | ILT Iterations Per **Long-Term** |
| řw | VMA Volumetric Measurement **A** | řy | VMB Volumetric Measurement **B** |
| ţw | P04 Position at 0 / 0 / **-Z** | ţy | P03 Position at 0 / 0 / **+Z** |

plus ten more positional pairs on the -Z/+Z contrast. SPT is the one
member of that family where -w and -y do not distinguish anything.

### A4. ANG's Type cell holds a spilled degree list

*Confirmed live upstream, verbatim.* The Type column for ANG (`dg`,
Angular Measurement) reads:

```
0* 1 arc-seconds 2 arc-minutes 3 mils 4 grads 5 degrees 6 points
7 hour angles 8 radians 9 sextants
```

The reference gives the type as plain `0*`. What follows it is a
second, differently-ordered assignment of the same nine units — the
reference and the spreadsheet both order the degrees points,
hour angles, grads, mils, radians, sextants, arc-seconds, arc-minutes,
degrees, while the stray text orders them arc-seconds, arc-minutes,
mils, grads, degrees, points, hour angles, radians, sextants. Which
ordering was meant is not recoverable from either source.

### A5. RESOLVED — fifty-four functional-group degrees recovered

GPB (`sļ`), GPC (`šḑ`), GPD (`šļ`), GPE (`zḑ`), GPF (`zļ`) and GPG
(`žḑ`) carry a C_S, an abbreviation and a description in the
spreadsheet, and nine empty degree cells apiece. They are not empty in
Quijada — his affix document populates all six — so this was a
transcription gap upstream, not a hole in the language.

All 54 are now extracted from the PDF by column geometry and written
into `data/data.json`, with a `RECOVERED_DEGREES` table in
`tools/sync_lexicon.py` that refills them whenever upstream is still
blank. No affix in the table has an empty degree list any more.

The extraction was validated against the three groups the sheet does
carry. **GPJ came back identical in all nine degrees.** GPA and GPH
differ only in that the sheet's versions are shortened — "alkyl halide"
against the PDF's "halo-, alkyl halide" — so the reading is faithful,
and fuller than what upstream holds. Quijada's own "..." placeholder
for the elided stem is kept as printed.

That leaves an inconsistency worth knowing about: GPA and GPH are still
the sheet's abbreviated forms while the other seven are now the PDF's
full ones. Replacing them is a one-line change to the same table if the
fuller wording is wanted throughout.

### A6. XCL is absent from the community spreadsheet

XCL (`çx`, External Standard for Comparison for Use with Levels) has a
full entry in `language_reference/affixes_reference.md` and no row in
the Collaborative Ithkuil IV Roots and Affixes Spreadsheet — *checked
live: the sheet has 527 rows, no XCL, and no affix at all with C_S
`çx`.* Commit 214744e restored it by hand, and `tools/sync_lexicon.py`
preserves locally-supplemented fields where upstream is blank, so a
re-sync will not drop it again.

This is the one entry where the two sources disagree by presence rather
than content, and the reference document is the fuller of the two.

### A7. ILT degree 7 reads "Eight"

*Confirmed live upstream.* ILT (`rţy`, Iterations Per Long-Term) runs
"X times per millenium", "per 10000 year period", "per 10⁵ year
period", "per age (10⁶ years)", "per epoch (10⁷ years)", "per era (10⁸
years)" — and then degree 7 is the bare word **Eight**, before degree 8
resumes with "X times per billion (10⁹) year period".

CYL, its sibling on the same scale (see A3), gives "occuring every 500
million years" at degree 7, which is where the 10⁸-to-10⁹ gap wants
filling. A stray cell has overwritten the entry.

Quijada's degree 7 is "X times per eon [5 x 10⁸ yrs.]". **Fixed** in
`data/data.json`.

## Lexicon

### L1. Five root clusters carry two unrelated meanings

| Cr | First meaning | Second meaning |
|----|---------------|----------------|
| ksmy | oven † | jagged line |
| nļt | groin undergarment | cicadomorphic bug |
| lzbḑ | psychodomorph (drain fly) | tabanid fly |
| rţnw | vitaceae 2 | rosoideae 7 |
| cfw | magnoliaceae | myristicaceae |

*Confirmed live upstream: these are the only five duplicated C_R among
5951 rows.* Only `ksmy` has a marker resolving the conflict: its first entry is
daggered, and the dagger is used throughout the spreadsheet for retired
words. The other four are two live homonyms apiece with no way to tell
them apart.

### L2. Two roots are phonotactically illegal

- `ňkhw` 'tetherball/sleight-of-hand skills contest' — §2.16 bars **ň**
  before **k**.
- `řẓňy` 'siphonapteran (flea) 2' — §2.16 bars **ň** before **y**.

*Both confirmed live upstream.* §2.16 gives its reasons in full (n
already assimilates to [ŋ] before
velars; \*ňy is indistinguishable from ny), and neither root has a
dagger, so these read as typos rather than as exceptions.

The other rule-violating roots in the lexicon are all retired:
`mps` 'relative clause head †', `mpš` 'framed relation †', `mpm`, `mpn`
and `mpx` violate §2.13, and every one of them is daggered. The rule
and the vocabulary agree everywhere except at L2.

Retired is not removed: the daggered entries are still in `data.db`,
so anything that walks the whole lexicon still meets them.
`fullparse/lexicon_sweep_test.go` builds a formative on every root and
checks the result is a legal word, and carries these four as a named
exclusion — which also means the list fails if one of them ever starts
validating, or leaves the lexicon.

## Corpus

The example sentences in `corpus/examples.txt` were transcribed and
glossed by a third party, not taken verbatim from Quijada. Defects
there are the transcription's, and are recorded separately for that
reason.

### C1. Two examples name a root the form does not contain

Cross-checking every root gloss in the corpus against the lexicon —
parsing each word, taking the C_R our parser finds, and comparing the
quoted root meaning in the aligned gloss segment against that C_R's
entry — 151 of 153 agree. The lexicon and the grammar's examples are
otherwise in step, including where the wording diverges: `bšt` glossed
'priest' against "religious leader", `lh` 'cousin' against "collateral
familiar relation", `tr` 'approach'/'go.away' against "linear motion",
`ňvy` 'apply.varnish' against "paint".

The two exceptions both involve the word for 'child', which is C_R
**l** at stem 2 ("human child"):

- **§4.8.4 / §6.1.3** *Weru'i*, glossed `'child'-G-VOC`. The C_R is
  `r`, whose stem 2 is "denying". G Perspective under a w- shortcut
  also needs a series-2 V_V, so the form the gloss describes is
  *Weilu'i*.
- **§5.1.6** *wesu*, glossed `[default CA]-stem2/prc-'child'-DPX-IND`.
  The C_R is `s`, the carrier root. The gloss is also inconsistent with
  itself: DPX is a Configuration, so a formative carrying it does not
  have a default C_A. The form the gloss describes is *elsu* — V_V `e`
  for stem2/prc, C_R `l`, C_A `s` for DPX, V_C `u` for IND — with no
  shortcut and no default C_A.

## Grammar

### G1. §2.24 bars çç and ļļ, and the morphology builds both

§2.24 prohibits the geminates **çç** and **ļļ**. §3.6.1 rule 4 then
geminates a sibilant "in any position" and gives **çkl → ççkl** as its
own worked example; rule 6 gives **tçkl → tççkl**. The bias-adjunct
table holds **pļļ** (CMD) and **kçç** (EXA). Thirty-nine words across
the official examples and the community corpus use one or the other,
including formatives whose geminated Ca marks the end of Slot V.

The narrowest reading that survives all of this is that §2 constrains
root and affix conjuncts, not forms the grammar itself derives — but
§2 opens by saying its restrictions hold "whether within the same
syllable or across adjacent syllables", with no such carve-out.

### G2. The §3.6 bn-substitution cannot cover both configurations that need it

Two Ca configurations reach an unsayable intermediate:

- MSS/A/DPL/RPV composes `tbn` and lands on `tḑ`, barred by §2.2 as a
  dental stop plus interdental.
- MDS/A/DPL/RPV composes `ţbn` and lands on `ţḑ`, barred by §2.5 as a
  homologous voicing mismatch.

§3.6 offers exactly one escape, **ţbn → (tḑ) → ḑy**, and it names the
second configuration on its input side and the first in its
intermediate. It cannot be read to cover both: they would then share
the surface `ḑy`, and Ca would stop being uniquely decodable. Whichever
reading is taken, one configuration is left with no pronounceable form.

The neighbouring rule **fbm → (fv) → vw** is parallel to the `ţbn`
reading — a fricative before bm/bn in both — which argues for the input
side. That still leaves MSS/A/DPL/RPV stranded.

### G3. WITHDRAWN — the conditions are footnotes, and our markdown lost them

Quijada's Ca table attaches its two alternate-form rules with
superscript markers:

```
GRA  GRADUATIVE   g / gz ¹        A  ABSTRACT   y (j)   n/ç²
DPL  DEPLETIVE    b / bz ¹
          ¹ Use the alternate form if the Configuration of the word is UPX
          ² Use the alternate form when preceded by [C]t-, [C]k-, or [C]p-
```

The Extension alternate is conditioned on UPX and the RPV Perspective
alternate on a preceding t/k/p — exactly the binding that composing all
3840 Ca values showed to be the only bijection. The source is correct.

`language_reference/morphology.md` dropped the superscripts and rendered
the two footnotes as free-standing sentences in page order, which reads
as though the Extension rule owns the t/k/p condition. The transcription has been fixed: `morphology.md` now carries the
footnote markers. See G33 for what the corrected table then shows.

### G4. WITHDRAWN — the substitution list is complete in the source

The §3.6 list in the PDF reads, in full:

```
pp mp    pb mb    rr ns    [C]gm [C]x    [C]bm [C]v
tt nt    kg ng    rř nš    [C]gn [C]ň    [C]bn [C]ḑ
kk nk    çy nd    řr ňs    ngn ňn        fbm (fv) vw
ll pļ             řř ňš    [C]çx [C]xw   ţbn (tḑ) ḑy
```

All four cells of the r/ř matrix are present — `rr ns`, `rř nš`,
`řr ňs`, `řř ňš` — so neither half of the old entry survives. Our
markdown lost the leading `r` of `rř` and dropped the `řř ňš` line.
Both are now restored, along with `çy`.

It also has `cy nd` where the source has **`çy nd`**, which settles a
question left open in the code: `allomorph/substitutions.go` is right
to substitute `çy`, and the markdown is what was wrong.

### G5. The gemination rules leave 115 of the 3840 Ca forms with no geminated form

§3.6.1 geminates the Ca complex to mark where Slot V ends, so every Ca
that can co-occur with a Slot V affix needs a geminated form. Running
the nine rules as written over all 3840 Ca values, 115 match no rule at
all. Each gap traces to a list that is narrower than the rule around
it.

**Rule 4's sibilant list omits the voiced affricates.** It names
"a sibilant fricative or affricate (**s**, **š**, **z**, **ž**, **ç**,
**c**, **č**)" — seven of the nine, leaving out **ẓ** and **j**. ẓ is
the MFF Configuration, so `ẓb`, `ẓg`, `ẓk`, `ẓl`, `ẓm`, `ẓn`, `ẓp`,
`ẓr`, `ẓt`, `ẓv`, `ẓw`, `ẓx`, `ẓy`, `ẓň`, `ẓř`, `ẓḑ`, `ẓkh`, `ẓph` and
`ẓth` all fall through. Rule 1's own example list geminates ẓ —
"**ẓ → ẓẓ**" — so the omission is in rule 4's parenthetical, not in the
phonology.

**Rule 6's fricative list omits five fricatives.** It covers a
voiceless stop followed by "(**s**, **š**, **f**, **ţ**, **ç**)". The
§1.1 chart has eleven fricatives; **v**, **ḑ**, **x**, **ļ** and **h**
are not on rule 6's list and are not sibilants, so `kv`, `kḑ`, `px`,
`pļ`, `tv` and `tx` have no rule. The h-forms are the widest of these:
the N/RPV Perspective alternate is **h**, so `th`, `ph`, `kh`, `kph`,
`kth`, `pkh`, `pth`, `tkh`, `tph`, `ẓkh`, `ẓph` and `ẓth` — twelve
forms — end in a fricative no rule can reach.

**Rule 7's table omits the voiceless-plus-voiced stop pairs.** The rule
covers "C_A forms ending in two stops" and supplies six substitutions:
pt, pk, kt, kp, tk, tp — every pair voiceless. But `kb`, `pg`, `tb` and
`tg` all arise (a stop Configuration plus the DPL or GRA Extension) and
are perfectly legal under §2.4, which names **tg** among its permitted
pairs. None has an entry.

**Rule 8's table omits ň.** The rule covers a form ending in "a stop
(t, k, p, d, g, b) plus nasal (**n**, **m**, **ň**)" and then supplies
twelve substitutions covering m and n only. `pň` and `tň` match the
condition and find nothing in the table.

**Rule 3 only reaches forms that begin with stop + liquid.** It says
"For forms **beginning with** a stop … followed by a liquid or an
approximant", which leaves the 99 Ca values of the shape
stop + stop + liquid — `kbl`, `kbr`, `kbw`, `kby`, `kbř`, `kpl`,
`kpr`, `kpw` and so on — matching neither rule 3 (they do not begin
with stop + liquid) nor rules 7 and 8 (they do not end in stop + stop
or stop + nasal).

Reading rule 3 loosely, as "a stop followed by a liquid anywhere in the
form", closes those 99 but not the rest: 40 forms still match no rule.
It is also not a free repair — the loose reading takes 300 further
forms away from rule 5 and geminates them somewhere else, so the two
readings disagree on the surface of `fkl` and its like, not just on
coverage.

None of this shows up as an ambiguity: the 3725 covered forms geminate
to 3725 distinct clusters, none of which collides with a bare Ca. The
system is sound where it is defined. It is simply not defined
everywhere, and a formative whose Ca is one of the 115 cannot take a
Slot V affix at all.

### G6. A moved C_N leaves the end of Slot V unmarkable

§3.8.1.2 lets a Pattern-1 Mood/Case-Scope C_N move out of Slot VIII
into the Slot VI C_A position, "thus shortening the word by one
syllable", on three conditions: Slot VIII's V_N is default MNO, the
C_N is something other than FAC/CCN -**h**-, and the C_A is the default
-**l**-. What lands in the C_A slot is then one of **hl**, **hr**,
**hm**, **hn**, **hň**.

That much is clean. None of the five collides with any of the 3840 C_A
forms or with any of their geminates — no C_A begins with h- at all —
and all five are permissible intervocalically under §5.2.

The problem is the other rule that owns Slot VI. §3.6.1 opens "If Slot
V contains any affixes, it becomes necessary to show where Slot V ends
and Slot VI begins", and accomplishes it by geminating the C_A form.
Run the nine rules against `hl`, `hr`, `hm`, `hn` and `hň` and none
fires: h is not a stop (rules 3, 6, 7, 8), not a sibilant (rule 4), not
on rule 5's list of non-sibilant fricatives (f, ţ, v, ḑ) nor a nasal,
not a liquid (rule 9), and the forms are not single consonants (rule
1).

Nor does §3.6.2 reach the case. It covers "the absence of a C_A form",
but scopes itself to a C_A "elided as per Sec. 3.1 and 3.2" — the Slot
I shortcut — and a formative using that shortcut has no Slot VI C_A for
§3.8.1.2 to replace in the first place.

So a formative with a Slot V affix, a default C_A, MNO Valence and a
non-FAC Mood has no way to mark where Slot V ends. Every one of those
conditions is an independent choice and nothing forbids the
combination. The document does consider §3.8.1.2's interactions
elsewhere — §3.9.1's Special Note suspends the V_C shortening rule when
§3.8.1.2 has been applied — so the omission here is of a piece the
author was otherwise tracking.

### G7. The Pattern-2 Mood FAC value is written "w/y" with no selection rule

The Pattern-2 Mood table gives FAC as `w/y` and says nothing about
which to use. No rule elsewhere in the document conditions the choice.

Attested usage does not settle it either. Both appear in identical
environments — `arţtuläwá` and `erčuläyá` differ only in the glide,
with the same Ca, the same Vn and the same Vk. Across the 63 instances
in the official examples the split is 51 w / 12 y; across the 445
instances in the community corpus it is 415 w / 30 y. Both forms are
attested after every plain vowel (a, ä, e, i, o, ö, u) and after
several diphthongs, so no phonological conditioning accounts for it.
The minority form is not rare enough to dismiss as a slip, and -ou-
inverts the ratio outright: 12 y against 7 w.

### G8. The NOMIC increment reproduces the Obv and PVS alternate forms

§4.6 gives each of the eleven referential categories three Effect
forms, and gives Obv and PVS a second form apiece "used in Referential
Affixes (see Sec. 4.6.5) to avoid ambiguity with geminated C_A forms":

| | NEUTRAL | BENEFICIAL | DETRIMENTAL |
|---|---|---|---|
| **Obv** | ll / **lç** | rr / **rç** | řř / **řç** |
| **PVS** | mm / **mç** | nn / **nç** | ňň / **ňç** |

The same section then says the NOMIC category is shown by adding
**-ç-** to a referent. Every one of the six alternates is what that
produces:

| Alternate | Also reads as |
|-----------|---------------|
| lç Obv/NEU | 1m/NEU `l` + NOMIC |
| rç Obv/BEN | 1m/BEN `r` + NOMIC |
| řç Obv/DET | 1m/DET `ř` + NOMIC |
| mç PVS/NEU | ma/NEU `m` + NOMIC |
| nç PVS/BEN | 2p/NEU `n` + NOMIC |
| ňç PVS/DET | pa/NEU `ň` + NOMIC |

`mç` is the worst of them, because §4.6 does not merely permit that
reading, it prescribes it: "The IPa and IPi Impersonal categories …
will instead be shown by adding the NOMIC affix above to the **ma** or
**mi** affixes." So in a referential affix, `mç` is both "whatever"
(PVS) and "one/someone" (ma + NOMIC).

Nothing rules the second reading out. §4.6.5 bars exactly one
increment — "a Referential affix cannot add the ABSTRACT Perspective
increments -**w** or -**y**" — and says nothing about NOMIC.
§4.6.4 does bar NOMIC, but only for Specialized Personal-Reference
Roots, which are a different construction. The alternates were
introduced to remove an ambiguity and introduce a different one.

### G9. The AGGLOMERATIVE increment is ambiguous with 2p/BEN plus AGGLOMERATIVE

AGGLOMERATIVE is shown by **-ļ-** or **-tļ-**. The second form exists
because a lone ļ has nowhere legal to stand — §3.1 bars it
word-initially and §5.1 bars it intervocalically — so `-tļ-` is the
repair.

But `t` is itself a referent, 2p/BEN. A referential affix ending
`-tļ` is therefore either *X* + AGGLOMERATIVE, or *X* + 2p/BEN +
AGGLOMERATIVE: `mtļ` is "he and co." or "he and you(pl., beneficial)
and co." Both readings are phonotactically fine and both are built by
rules the same paragraph gives.

### G10. Most bias adjuncts are not permissible words

A bias adjunct (§4.7) is a bare consonant conjunct standing alone as a
word — a shape §1.4 and §3-§4 never contemplate, since every rule
there is written about a conjunct with a vowel-form beside it (§4.1
opens "A single word-final consonant **following a vowel-form**…").
Taking the word-initial inventory at its word, 34 of the 61 forms are
not licensed. These are not hypothetical words: every one of them is
attested as a standalone word in the community corpus — `pļļ` 291
times, `msf` 127, `kçç` 48, `cč` 33, `ļļč` 29, `lst` 13, `lf` 12,
`rrj` 10.

- **§3.2.9** — "Word-initial liquids l- and r- may be followed by the
  semiconsonants -w or -y", and by nothing else in §3: ACC `lf`,
  ADS `lļ`, ANP `lst`, FOR `lzp`, ISP `lçp`, SGS `ltç`,
  SKP `rnž`, and via §6.3.1 the geminates PPX `llh`, RFL `llm`,
  TRP `llč`, EXG `rrs`, CNV `rrj`.
- **§3.2.8** — word-initial nasals take a liquid or an approximant:
  ATE `ňj`, CRR `ňţ`, ACH `mçt`, MAN `msk`, RSG `msf`, and the
  geminates DUB `mmf`, GRT `mmh`, IRO `mmž`, RVL `mmļ`, PSM `nnţ`,
  SOL `ňňs`.
- **ř-** is granted no word-initial pair at all — §3.2.9 covers l- and
  r- only: APB `řs`, DOL `řřx`, IVD `řřn`.
- **§3.2.5** — word-initial f, v, ţ, ḑ take a liquid, approximant,
  nasal, or a stop/affricate *of the same voicing*: APH `vvz`,
  EUP `vvt`, DPB `ffx`.
- **§3.2.1** — a word-initial sibilant fricative takes a consonant of
  the same voicing: PSC `žžt` pairs voiced ž with voiceless t.
- **§3.2.2** — a word-initial sibilant affricate "cannot be followed
  by … another affricate": DFD `cč`. §2.5 permits `cč` in general and
  names it as an example, so this form is legal everywhere except at
  the start of a word, which is the only place a bias adjunct puts it.
- **§3.3** — a stop plus a non-sibilant fricative admits only -w or -y
  as a third consonant: DRS `pfc`.
- **§3.3.4** — a sibilant plus a same-voiced stop admits a liquid or
  approximant third, and ļ is neither: RPU `šštļ`.

SAT is a further case, and its form is unsettled: the grammar prints
`lţ` and `data/data.json` has `ļţ`, with the community corpus using
both (35 `ļţ` against 21 `lţ`). Neither is licensed — `lţ` falls to
§3.2.9 with the rest of the l-initials, and `ļţ` to §3.2.6, which lets
a word-initial ļ- take "a voiceless stop, a voiceless affricate, a
nasal, or -w or -y" and not a fricative.

ARB `xtļ` is a further case: §3.2.3 licenses `xt` word-initially but
§3.3 grants no triple beginning with x-, so the form is neither
permitted nor prohibited.

Four more — CMD `pļļ`, EXA `kçç`, OPT `ççk` and STU `ļļč` — are the
§2.24 geminates of G1 above.

### G11. §4.7's stated design principle contradicts its own table

§4.7 says each bias adjunct is "phonologically structured to contain
continuant and sonorant consonants so that … they may be pronounced in
an exaggerated, prolonged fashion". Thirty-five of the 61 forms contain
a stop or an affricate, and seventeen *begin* with one — ANN `drr`,
CMD `pļļ`, CRP `gžž`, CTP `kšš`, CTV `gvv`, DCC `gzj`, DFD `cč`,
DIS `kff`, DLC `ẓmm`, DRS `pfc`, EUH `gzz`, EXA `kçç`, MNF `pss`,
IDG `pšš`, PES `ksp`, RAC `kll`, VEX `ksk`. An onset of `drr` or `ksk`
is the one thing that cannot be prolonged.

The very next sentence concedes the point in passing — "those ending in
a voiceless stop or voiceless affricate may aspirate or even
ejectivize" — without reconciling it with the claim it follows.

### G12. §1.2.1's vowel inventory does not cover the conjuncts the morphology uses

§1.2.1 gives ten permissible diphthongs — ai, ei, ëi, oi, ui, au, eu,
ëu, ou, iu — and describes every other two-vowel sequence as a
"disyllabic conjunct". But the morphology's own vowel-form tables use
**ae**, **ea**, **üo** and **üö** as form-0 values, and §4.6.3 calls
**üo** a "word-initial diphthong" outright. Under §1.2.1 it is not a
diphthong, and the tables never say how many syllables these forms
carry — which matters, because syllable count is what selects the
formative's Relation in Slot X.

### G15. Three cross-references point at the wrong section

- §3.9.3 closes by saying "Sec. 3.9.3.4 below provides the Slot IX V_K
  affix values denoting these categories". There is no §3.9.3.4; the
  subsections stop at 3.9.3.3. The V_K values are in fact in §3.9.3.2,
  the section the same sentence has just described as covering
  Validation.
- §3.9.1 says "The phonological structure of case-accessor affixes is
  shown in Sec. 3.9.3". §3.9.3 is V_K — Illocution and Validation. The
  case-accessor affixes are §3.9.2.
- §4.9 says "adherence to the parsing rules in Sec. 2.1". §2.1 is the
  slot-structure table for a formative without a Slot IV/VI shortcut;
  the parsing rules are §2.3.

Every other "Sec. N.N" reference in the document resolves to a heading
that exists.

### G16. Gradient type is not determined by C_S shape, though it nearly is

§3.5.0.1 opens: "Each affix's nine degrees follow one of seven
'gradient types' **determined by the phonological shape of the C_S
consonant form**", and then gives a shape description for each type.
Classifying all 527 affixes by those descriptions and comparing against
their declared types, 512 of the 526 typed affixes sit in a shape-class
that maps to more than one type. Taken literally the claim is false —
but the way it fails is specific, and three separate things have to be
supplied before it comes right.

**Type 0 is not a shape class.** Its own text gives the game away:
"The nine degrees represent an arbitrary list of nine related but
non-graduated concepts". That is a semantic property, and it is
assigned to 221 affixes spread across *every* shape class — 27 that
begin with r/ř (A1's shape), 9 ending in -m or -x (D2's), 4 containing
ç (B's), 3 ending in -ř (C's), 2 ending in -h (A2's), and 33 matching
no shape rule at all. Its stated shape covers only 171 of the 221.
Setting Type 0 aside as semantically assigned leaves 271 affixes for
the shape rules to determine.

**Type 0's third clause overreaches.** It reads "or is a bi-consonantal
form ending in a sibilant", which catches sibilant + sibilant pairs and
so claims 14 affixes that are declared D1 — `šč`, `cj`, `žč`, `sc`,
`sj`, `zj` and their like. The preceding clause already covers a
sibilant followed by a stop or non-sibilant fricative, so the third is
evidently meant for a *non*-sibilant followed by a sibilant. Read that
way, the 14 stop being misclaimed.

**The rules need a priority order, and none is given.** Thirty-seven
C_S forms match two or more shape rules: `rř` is both A1 and C, `rx`
and `rm` are both A1 and D2, `rç` is both A1 and B, `çx` is both B and
D2, and 16 forms are both Type 0 and A1. Only A2 states a precedence
("except -rh and -řh, which are Type A1"). The declared types imply
C > A1 > B > D2 consistently, but the document never says so.

With those three supplied, the shape rules become a function on 268 of
the remaining 271 affixes. The three that stay out:

- **AUT** `pč`, declared **B**. Type B is "two contrasting states …
  with a neutral midpoint"; AUT's nine degrees are nine unrelated
  grounds of authorization — personal privilege, natural right,
  statute, custom, favour, threat, rank, class, governing entity — with
  no contrast and no midpoint. And `pč` contains no ç, which is B's
  entire shape condition. Both shape and semantics say Type 0.
- **COO** `ň`, declared **C**. Type C requires C_S to end in -ř and
  means "a scale of anticipated-to-unanticipated effect". COO's degrees
  are nine flavours of "and" — shared topic, shared participant, in
  sequence, at the same time — which is neither a scale nor about
  anticipation, and `ň` does not end in -ř.
- **NEG** `r`, declared **D1**. Here the declaration is right and the
  rule is wrong: NEG runs from relative negation (1-4) through
  "neither the preceding nor X" (5) to absolute negation (6-9), which
  is exactly D1's "standard spectrum from one extreme through a
  midpoint to the opposite extreme" and not A1's "gradient from
  zero/none to maximum". `r` is the one affix where A1's shape rule
  demonstrably picks the wrong type.

### G17. WITHDRAWN — the PDF publishes all fourteen increments

Our markdown carried eight, not Quijada. See G34. What survives of
this entry is the observation at the end, that none of the fourteen
increments collides with an ordinary affix C_S.

The original claim rested on the prose:

> There are two separate C_S increments for each of the seven types of
> affix (Types-1, -2, and -3 Case-Accessor, Types-1, -2, and -3 Inverse
> Case-Accessor, and Case-Stacking Affix), the first C_S increment
> being used for Cases 1 through 36, while the second C_S increment is
> used for Cases 37 through 68.

Seven types times two increments is fourteen, and the PDF's table
supplies all fourteen. Ours supplied eight.

What is published is otherwise sound. The V_X carries the case-group in
its series and the case within the group in its form — four series by
nine forms for cases 1-36, by eight for cases 37-68 with vowel-tier 8
unused, giving exactly 36 and 32. And none of them collides with an
ordinary affix: all fourteen are absent from the 527-affix table,
though twenty other two-consonant forms ending in -w or -y are taken.

### G18. Every referential consonant is also an ordinary affix C_S

§4.6.5 lets an otherwise-empty Slot V or VII hold a "Referential
affix": one of the 33 referential consonant-forms with a Type-3 V_X.
All 33 of those forms are also C_S forms of ordinary affixes — not
most, all:

| | | | |
|---|---|---|---|
| l = CTR | r = NEG | ř = IOR | s = CMF |
| š = SEX | ž = CPC | n = TPF | t = DCD |
| d = SCS | m = EFE | p = P05 | b = DEV |
| ň = COO | k = P06 | g = EXN | z = XX3 |
| ţ = P01 | ḑ = P02 | ẓ = PLA | f = P07 |
| v = P08 | c = GID | č = SWR | j = X10 |
| th = QUA | ph = PCN | kh = SBT | lç = SID |
| rç = PEB | řç = BCD | mç = DCF | nç = MCF |
| ňç = VRF | | | |

Most of the overlap costs nothing, and that appears to be the design.
§3.5 defines Type-3 as applying "to previous C_S V_X / V_X C_S affix
only (or the following affix if it is the first in the slot)", so a
Type-3 affix with nothing adjacent is already meaningless — its slot is
free to be repurposed, which is precisely what §4.6.5 does.

What the document does not settle is the cross-slot case. §4.6.5's
trigger is "a lone Type-3 V_X C_S affix without any adjacent Type-1 or
Type-2 affix **for it to apply to**". If Slot V holds affixes and Slot
VII holds a single Type-3 affix, §3.5 says that affix applies to the
previous one — which is in Slot V. So it does have something to apply
to, and by the letter of the trigger the referential reading is
blocked; but nothing says whether a slot boundary interrupts
adjacency. Since the 33 collisions include such common affixes as NEG,
DCD, SWR, COO and SEX, which reading holds decides a great many words.

### G19. MOSTLY WITHDRAWN — three of the four claims were ours

*Re-read against the source.* §8 is not a V4 document at all: Quijada's
grammar ends at Chapter 7 and says nothing about numbers. The chapter
is our condensation of Chapter 13 of the 2011 grammar,
`ithkuil.net/newithkuil_13_numbers.htm`. Three of the four
disagreements this entry reported were introduced by that
condensation, not found in the source.

**Withdrawn: the coordinative affix's label.** The entry read our
"-iň (COO/1)" as COO degree 1 and observed that `-iň` is degree 4. The
source writes it `-Vň/1 (= -iň)`. The V is a placeholder for the
degree vowel, so the /1 is the affix *type*, not the degree — the two
notations cannot both be degrees. The source is consistent, and our
rewrite created the mismatch by resolving V to i while keeping the /1.

**Withdrawn: the affix appears in no example.** It appears in the
source's longest example, twice — *walẓorsiň* and *zalëirsiň* in
727,903,533,460. We dropped that example when condensing to four, and
the entry then reported the absence as the source's.

**Withdrawn in part: §8.2 against itself.** Our text said the roots for
1 to 99 are based on "roots for 0 through 10"; the source says "1
through 10". A narrower disagreement does survive: "1 through 10"
against the TNX note's "used with the number roots 0 thru 9 to create
the numbers 11 through 99". Only the affix note's version works, since
admitting root 10 would generate 20, 30 ... 100 a second way, with the
last colliding with the dedicated root for 100, `-GZ-`.

**Stands: §8.1 against §8.2.** §8.1 calls the numbers from zero to 100
"autonomous units represented by single stems", and §8.2 builds
everything from 11 to 99 as a root plus the TNX affix. 89 of the 101
are not single stems.

Everything else in §8 checks out. All twenty number roots match the
lexicon (`vr` zero, `ll` one ... `čg` ten-quadrillion, plus `cg` `jd`
`ļj` `bc` `ţẓ` for bases 11-15); the powers are right (100² = 10,000,
100⁴ = 10⁸, 100⁸ = 10¹⁶); TNX's nine degrees are +10 to +90 as stated;
and the examples' case marking follows the rule, with PARTITIVE `-ui`
on *gzalui* and *wapcui* and COMITATIVE `-ë'i` on *wansorsë'i*.

### G20. The modular adjunct's mandatory Slot 4 has no way to say which category it is

§4.3 lays the adjunct out in four slots and says "Slots 1 and 4 are
mandatory; the other slots are optional". Slot 4 holds "Aspect or
Valence/Phase/Level/Effect or Specialized Scope", and the row beneath
gives its content as "V_N or V_H" — a bare vowel, with no consonant.

That is a problem, because a V_N vowel does not by itself say which
category it belongs to. The four Pattern-1 categories and the four
Aspect columns share the same vowel forms one for one: series 1 is
Valence *or* Aspect column 1, series 2 is Phase *or* column 2, series 3
is Effect *or* column 3, series 4 is Level *or* column 4. In formative
Slot VIII the following C_N resolves it — Pattern 1 (h, hl, hr, hm, hn,
hň) means Valence/Phase/Effect/Level, Pattern 2 (w/y, hw, hrw, hmw,
hnw, hňw) means Aspect. Slot 4 of the adjunct has no C_N.

The adjunct's other two slots both carry the distinction. Slot 2 has
its C_N and so inherits the formative's mechanism. Slot 3 is given a
consonant for no other purpose: "C_M = **n** if V_N represents an
Aspect, otherwise C_M = **ň**". Slot 4 — the mandatory one — is the
only one left without a marker, and the omission is visible in the
document's own example list: `uhlaini` ends in a Slot-4 `i`, which is
RCP Valence or PRG Aspect with nothing to choose between them.

Stress does not help. §4.3 says the Slot 4 vowel is read as V_H,
the specialized-scope value, only "if the adjunct has ultimate stress",
so penultimate stress already means "this is a V_N" and cannot also
encode which V_N.

### G21. The PHR adjunct's example list breaks its own paradigm

§4.5's four suppletive adjuncts are given identical example lists,
differing only in the C_P:

| | | | | | | |
|---|---|---|---|---|---|---|
| CAR `hl` | hla | hlei | **hloa** | hle'e | hla'u | hli'a |
| QUO `hm` | hma | hmei | **hmoa** | hme'e | hma'u | hmi'a |
| NAM `hn` | hna | hnei | **hnoa** | hne'e | hna'u | hni'a |
| PHR `hň` | hňa | hňei | **hňo** | hňe'e | hňa'u | hňi'a |

The six V_C values are THM `a`, GEN `ei`, SIT `oa`, COR `e'e`, ASI
`a'u` and LOC `i'a` — chosen to exercise one case from each of §1.7's
insertion patterns, which is why the same six recur four times. PHR's
third entry is `hňo`, whose V_C is `o`, ERG. The paradigm calls for
`hňoa`.

### G22. §4.2's lead-in was not updated when its table was

The version history for v1.3 (see `CHANGELOG.md`) records the change:

> **Sec. 4.2:** The Slot IV V_R Values for the Specialized C_S-Root now
> show Degree plus Context, instead of Degree plus Specification, as it
> did not make sense for these Specialized C_S-Roots to show any
> Specification other than BSC given that Specification does not apply
> to V_X C_S affixes.

The table was updated — its header reads "showing Affix-degree for the
Slot III C_S-form plus Context" and its four columns are EXS, FNC, RPS
and AMG — and so was the closing note, "these Specialized C_S-roots are
considered to have BSC Specification only". The sentence that
introduces the table was not:

> The Affix-Degree and **Specification** of the Specialized C_S-root is
> shown by the V_R value in Slot IV:

A reader who takes that sentence at face value will look for
Specification in a table that does not encode it.

### G23. §4.4 still refers to a "CAR adjunct hü" that no longer exists

v1.3 relabelled the Carrier-End Register Adjunct from **CAR** to
**END**, and reassigned **CAR** to the Carrier Suppletive Adjunct of
§4.5.1. §4.4's table reflects that: the END row reads "CARRIER-END: end
of term/phrase governed by carrier stem/adjunct", with no initial
adjunct and a final adjunct of `hüi`. The note directly beneath it does
not:

> Since Sec. 1.5 external juncture rules do not apply to foreign
> names/words, insert a pause after uttering the name/words prior to
> the **CAR** adjunct *hü*, or pronounce the last word of the proper
> name/phrase with low tone.

Both halves are stale. **CAR** now names §4.5.1's adjunct, which is
`hl` + V_C and never `hü`; and `hü` itself occurs exactly once in the
document, in this sentence. What the note describes — pausing before
the marker that closes a carrier phrase — is the END adjunct `hüi`.

### G24. Every version-history pointer into §3.9.3 is one subsection off

Three changelog entries name a §3.9.3 subsection that does not contain
what they describe:

| Entry | Says | Actually in |
|-------|------|-------------|
| v1.3 | "**Sec. 3.9.3.3:** The presentation of V_K Illocution and Validation phonological values has been simplified" | §3.9.3.2 |
| v1.3.1 | "**Sec. 3.9.3.1:** The table providing descriptions of the nine validations has been reinstated" | §3.9.3.2 |
| v1.3.2 | "**Sec. 3.9.3.3:** The 3-letter abbreviation for VERIFICATIVE Illocution has been changed to **VER**" | §3.9.3.1 |

§3.9.3.1 is Illocution, §3.9.3.2 is Validation and carries both V_K
tables, and §3.9.3.3 is "If desired to show V_K information on a FRAMED
formative or a concatenated formative".

Read with G15 — where §3.9.3's own closing sentence promises that
"Sec. 3.9.3.4 below provides the Slot IX V_K affix values" and no such
subsection exists — the pattern suggests §3.9.3 once had four
subsections, that the V_K value tables had one of their own, and that
it was merged into the Validation section without any of the four
pointers into the range being renumbered.

Every other version-history entry checks out: MTH is gone from §4.4's
register table and END is there; §4.5's four adjuncts carry the labels
CAR, QUO, NAM and PHR; the Mood/Case-Scope adjunct is gone and MCS
exists as an affix (`bẓ`, "Mood and Case-Scoping"); and §4.7 has ADS,
MNF MANIFESTIVE and RSG RESIGNATIVE with no EXPERIENTIAL or
RENUNCIATIVE bias remaining.

### G25. Both specialized root types drop Stem from Slot II without saying so

Slot II of a standard formative encodes Stem and Version — eight
values, four stems by two versions. Both specialized root constructions
replace that table and neither mentions what becomes of Stem:

- §4.2's Specialized C_S-Root uses four Slot II values (`ëi`, `eë`,
  `ëu`, `oë`) encoding Version by Function. Function has moved up from
  Slot IV to make room for Affix-Degree there; Stem has simply gone.
  §4.2 is careful to settle Specification — "considered to have BSC
  Specification only" — and says nothing at all about Stem.
- §4.6.4's Specialized Personal-Reference Root uses two Slot II values
  (`ae`, `ea`) encoding Version alone, and states that "Slot IV values
  for these Specialized Personal-Reference roots are the same as for
  standard formatives, showing Function, Specification, and Context".
  So Stem has no home in either slot.

§4.6.4 then writes as though stems were still available: "the meaning
of **each stem** of the Specialized Personal-Reference Root changes
depending on its Perspective", and the table below it is headed "Stem 1
Nominal meaning" and "Stem 1 Verbal meaning" — a label that only means
something if Stems 2 and 3 can also be expressed.

### G26. Five CHC examples label a degree-1 vowel as degree 2

§5.8's worked sentences insert the CHC affix (C_S = **rz**) at four
different degrees, and the Type-1 V_X series is a, ä, e, i, ëi, ö, o,
ü, u. Three of the four degrees are written correctly:

| Label | Vowel used | Vowel required | |
|-------|-----------|----------------|---|
| CHC1/3 | e | e | *Ellyulerza*, *Ellyalerza*, *welacerzooe* |
| CHC1/7 | o | o | *welacorzooe* |
| CHC1/9 | u | u | *welacurzu* (twice) |
| **CHC1/2** | **a** | **ä** | *welacarzulwu*, *welacarzu*, *Etxularza*, *welecarzu*, *Adcsularzeuha* |

`a` is degree 1, not degree 2. All five CHC1/2 examples use it.

The English gloss suggests the labels are what slipped rather than the
forms. Degree 1 is "can do nothing to stop it, initiation is
inevitable"; degree 2 is "chooses to acquiesce due to being okay with,
or indifferent as to outcome". Three of the five sentences are *The boy
is made to jump*, *Someone's being made to jump* and *Being made to eat
is taking place* — compulsion, which is degree 1. Reading them as
CHC1/1 makes form and meaning agree at once.

This survived two rounds of correction: v1.3 revised §5.8's examples
and v1.3.1 corrected "a few typos in the example sentences".

The notation is otherwise dependable. Sweeping every affix degree label
in the document and checking the vowel that precedes its C_S in the
accompanying Ithkuil word, 40 of the 45 that could be aligned are
exactly right; the five failures are these.

### G29. The 0* marker cannot mean what §3.5.0.1 says it means

§3.5.0.1 introduces the marker in one clause: "Some Type-0 affixes are
marked **0\***, indicating an associated C_R root form." Read as an
existence claim it is uninformative, because it is true of almost every
affix:

| | affixes | whose C_S is also a root C_R |
|---|---|---|
| marked with a star | 36 | 34 (94%) |
| unmarked | 491 | 468 (95%) |

The root lexicon has 5946 entries over the same short consonant
clusters the affixes use, so a same-cluster root nearly always exists.
Whatever the star distinguishes, it is not that.

Read instead as a claim about a *semantically* associated root, it
holds for most of the marked affixes — CNQ `řv` "Degree of
Consequentiality" beside the root "consequence/outcome/result", EXT
`řḑ` "Exactness of Identity" beside "exactitude", FLS `mh` "Degree of
Fluctuation/Stability" beside "change/stability/fluctuation" — but
four of them point at a root with no relation to the affix at all:

| Affix | C_S | Affix meaning | Root at that C_S |
|-------|-----|---------------|------------------|
| ANG | dg | Angular Measurement | weight/mass |
| GID | c | Gender Identity | hearing/sound |
| PCM | bḑ | Primary Construction Material | cucumis 1 |
| TNX | rs | Multiples of Ten | sincerity/honesty/guilelessness |

Either the star on these four is wrong, or it points at a root the
tables never name.

Separately, the marker **D1\*** occurs once, on NEW (`sp`,
Newness/Revision), and is defined nowhere. §3.5.0.1 attaches the star
to Type 0 alone. NEW's root at `sp` is "degree of newness", so the star
is evidently doing the same work on a D1 affix — work the document does
not sanction.

### G30. PHS degree 4 is coded IMT where the category is ITM

Ninety category codes are embedded in affix degree descriptions in the
form "(CODE) Full Name" — the MCS, PHS, AP1-4, IVL, LVL and VAL
affixes, which take grammatical categories as their degrees rather than
lexical meanings. Eighty-nine resolve to an abbreviation in the grammar
tables. One does not:

| PHS degree | Affix table | Phase category |
|------------|-------------|----------------|
| 3 | (REP) Repetitive | REP Repetitive |
| **4** | **(IMT) Intermittent** | **ITM** Intermittent |
| 5 | (RCT) Recurrent | RCT Recurrent |

Every other degree of PHS matches the Phase table exactly and in order,
and the name "Intermittent" is right, so `IMT` is a transposition of
`ITM`. The grammar document uses ITM throughout — in the Slot VIII
Pattern-1 table and in the writing-system chapter — and IMT appears
nowhere in it. The affix table is the only place the transposed form
occurs. **Fixed** in `data/data.json`.


### G32. WITHDRAWN — the script document tabulates 28, as it says

*Concerns the writing system, which is no longer covered here; kept for
the record.*

Quijada's script document says "The 28 forms below are the 'core'
characters" and then lists exactly 28: p b f v s z c ż / t d ţ ḍ š ž č
j / k g x l r ļ ř / m n ň ç h. The two consonants missing against the
§1.1 inventory of 30 are **w** and **y**, the semiconsonants, which the
script handles elsewhere.

Our markdown's table had added w and y, making it disagree with the
count printed beside it. It now lists 28.

### G13. The documents disagree on how to write ẓ and ḑ

§1.1's phoneme chart gives the affricates as **c ẓ č j**, and §1.3 lists
the sanctioned alternate spellings: ţ may be written ṭ or ŧ, ḑ as ḍ or
đ, ň as ṇ or ŋ, ř as ṛ or ṙ, ļ as ł or ḷ. **ẓ is not on that list** — it
has no sanctioned variant. The documents use one anyway:

| | ẓ (U+1E93, dot below) | ż (U+017C, dot above) |
|---|---|---|
| morphology.md | 10 | 0 |
| phonotactics.md | 0 | 23 |
| affixes_reference.md | 31 | 0 |
| data.json | 162 | 0 |

Every ẓ in the phonotactics document is written ż, including in §2.2's
roster of the sibilants and in §2.5's list of prohibited conjuncts —
the places a reader goes to learn which characters the language has.
morphology.md keeps ẓ throughout. (It held one ż, in the script
chapter's table of core characters; that chapter is no longer here.)

ḑ has the same problem in miniature, and there the variant is at least
sanctioned: the §4.6 referential table gives the mi/DETRIMENTAL form as
**đ**, the only place in the document that exercises §1.3's alternate,
and a reader working through the tables meets a character that appears
nowhere else and is absent from §1.1 as printed.

Either way a machine reader checking the phonotactic rules against the
phoneme inventory sees an unknown codepoint twenty-three times.

### G14. WITHDRAWN — the V_K diacritics are correct in the source

Quijada's Validation table gives REC as **â** and USP as **êi** — the
forms §1.3.1's rules require, and the ones this entry predicted. Our
markdown had `à` (a-grave) and a bare `ëi`; both are now corrected.

Both values were derived from the vowel-form series and the
acute/circumflex convention before the PDF was available, and both
turned out to match it.

### G27. WITHDRAWN — CLG is -ḑc in the source

The affix document and §7.0 of the grammar both give **-ḑc**. Our
markdown lost the cedilla, producing `-dc-`, which is what §2.2
prohibits. `data/data.json` already had `ḑc`; the markdown is now
corrected too.

### G28. §7.2 writes OCG for OGC

§7.0 defines the affix as **OGC**, Orientation relative to a
Geographic Central point, and the affix table agrees. §7.2 refers to
"the Type-2 **OCG**" when explaining that the Southern Ocean takes the
affix directly rather than a carrier stem. The two letters are
transposed; OCG is not an affix.

Otherwise §7 holds up. All 489 romanized forms in §7.1 through §7.7 use
only characters from the §1.1 inventory, which is what §7.0 requires of
them — it waives the phonotactic rules for proper names ("Ithkuil
phonotactic restraints do not apply as long as the name is
pronounceable") but not the phoneme inventory. OGC's nine degrees match
the affix table exactly and form the antipodal arrangement its D1
gradient type calls for: 1 northern against 9 southern, 2 northwestern
against 8 southeastern, 3 western against 7 eastern, 4 northeastern
against 6 southwestern, with 5 geographically central at the midpoint.
And *usarcsaidna amerika* / *usarcsuidna amerika* use OGC at Type-2
degrees 1 and 9 — `ai` and `ui` — which is northern and southern, as
North and South America require.

### G31. WITHDRAWN — the DES bias is mřř in the source

§4.7 of the PDF reads `DES DESPERATIVE mřř`. Our markdown has `mřr`,
which §2.21 forbids. The corpus evidence that pointed this way — 42
standalone `mřř` against one `mřr` — was right, and the source
confirms it. The markdown is now corrected.

### G33. The UPX footnote is marked on two of the five Extension rows

Quijada's Ca table conditions its alternate forms with two footnotes,
and the ¹ marker sits on only two Extension entries:

```
PRX  PROXIMAL     t/d              M  MONADIC        — (l)   l (tļ)
ICP  INCEPTIVE    k/g              G  AGGLOMERATIVE  r       ř
ATV  ATTENUATIVE  p/b              N  NOMIC          w (v)   m/h²
GRA  GRADUATIVE   g / gz¹          A  ABSTRACT       y (j)   n/ç²
DPL  DEPLETIVE    b / bz¹
       ¹ Use the alternate form if the Configuration of the word is UPX
       ² Use the alternate form when preceded by [C]t-, [C]k-, or [C]p-
```

PRX, ICP and ATV carry no marker. The ² is on both rows that need it,
and it is captured on the same text lines as the unmarked Extensions,
so this is not an artifact of reading the PDF — where a superscript
exists on those lines it comes through.

Read literally, only GRA and DPL take their alternate under UPX. That
costs 96 distinctions:

| Reading | Distinct Ca forms | Colliding |
|---------|------------------|-----------|
| ¹ governs the whole Extension column | **3840** | **0** |
| ¹ governs GRA and DPL only, as printed | 3744 | 96 |

The collisions are exactly what the alternate exists to prevent. UPX
contributes no Configuration consonant, so UPX/PRX composes a bare `t`
— which is already MSS/DEL. Likewise `k` is both UPX/ICP and MSC/DEL,
and `p` is both UPX/ATV and MSF/DEL, across every Affiliation and
Perspective.

So the footnote has to govern the column, and the marker belongs on all
five rows. This is the residue of the old G3: the conditions were never
swapped, but the ¹ is under-applied.

### G34. Our markdown lost three of the seven case-accessor affixes

*A transcription defect, in layer 3 rather than in Quijada.* §3.9.2
names "two separate C_S increments for each of the seven types of
affix", and the PDF's table gives all fourteen. Our markdown carried
four of the seven columns and eight of the fourteen increments:

| Affix | Quijada, cases 1-36 | Our markdown |
|---|---|---|
| Case-Accessor, Type-1 | sw | sw |
| Case-Accessor, Type-2 | **zw** | *lost* |
| Case-Accessor, Type-3 | čw | čw, mislabelled Inverse Type-1 |
| Inverse Case-Accessor, Type-1 | šw | šw, mislabelled Type-3 |
| Inverse Case-Accessor, Type-2 | **žw** | *lost* |
| Inverse Case-Accessor, Type-3 | **jw** | *lost* |
| Case-Stacking | lw | lw |

Same for the -y series: `zy`, `žy` and `jy` were missing and the
remaining labels were shifted. The header spans seven affix kinds over
merged cells, and the markdown conversion collapsed the merges, so the
four surviving C_S values were re-dealt across the wrong columns.

The prose said "seven" the whole time, which is what makes the table
self-evidently short — a count in the text against a count in the
table is the cheapest check there is, and it was available without
consulting the source at all.

**Fixed** in `language_reference/morphology.md`, which now lists the
fourteen increments as a flat table plus a separate series-to-case-group
table, rather than reproducing the merged-cell layout that caused this.

### G35. The documents disagree on whether ç is a fricative or an affricate

The two source documents print the same phoneme in different rows of
their phonemic inventories.

The grammar document, §1.1, puts it in the fricative row under PALATAL,
leaving the palatal affricate cell empty:

```
FRICATIVE   f v      ţ ḑ  s z      š ž    ç      x    h   ļ
AFFRICATE                 c ẓ      č j
```

The phonotactics document does the reverse: the palatal fricative cell
is empty and `ç` sits in the affricate row. This is not a column
artifact of reading the PDF. In the bounding-box dump the PALATAL
column spans x 404-430, `ç` sits at x 414 on the affricate row, and
the fricative row jumps from `ž` at 384 straight to `x` at 447.

The grammar document is right, and the phonotactics document agrees
with it everywhere except that one cell: its own §2.10 opens "the
voiceless palatal fricative -ç-", and §3.2 lists `pç`, `tç` and `kç`
among the stop + *non-sibilant fricative* conjuncts.

### G36. The phonotactics document contradicts itself about ç being a sibilant

Its opening paragraph defines both terms to include `ç`:

- "the term 'sibilant' refers to -s-, -z-, -š-, -ž-, -c-, -ż-, -č-, -j-, and -ç-"
- "the term 'sibilant fricative' refers to -s-, -z-, -š-, -ž-, and -ç-"

Five later rules take the opposite view, three of them by writing the
membership out:

| Rule | Text | ç a sibilant? |
|------|------|---------------|
| §2.2 | "any sibilant (s, z, š, ž, c, ż, č, j)" | no |
| §2.10 | "a sibilant fricative (s, z, š, ž)" | no |
| §2.17 | "any sibilant fricative (s, z, š, ž), -ç-" | no |
| §3.2 | `pç`, `tç`, `kç` as *non-sibilant* fricatives | no |
| §3.2.1 | "a word-initial sibilant fricative (s, z, š, ž)" | no |
| §3.3.4 | "Word-initial sibilant fricatives (s, z, š, ž, ç)" | yes |

§2.2 settles it. It forbids a dental stop before any sibilant, and
§3.2 lists `tç` as permissible; the two can only both hold if `ç` is
not a sibilant. §2.10 and §2.17 also become redundant under the
opening definition, since §2.8 already forbids adjacent distinct
sibilant fricatives.

So the opening paragraph is wrong in both clauses, and §3.3.4 wrong
with it. `validation` reads `ç` as neither a sibilant nor a sibilant
fricative, which the corpus test corroborates.
