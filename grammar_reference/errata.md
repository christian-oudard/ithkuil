# Errata in the V4 Source Material

Defects in the published grammar, the affix tables and the community
lexicon — not in this repository's code. Each entry states what the
sources say, how the conflict was established, and what it costs a
reader trying to implement the language.

Two independent renderings of the affix table are available and are
compared throughout: `grammar_reference/affixes_reference.md`, which
predates the spreadsheet sync, and `data/affixes.tsv`, mirrored from
the community spreadsheet. Where they agree, the defect is Quijada's.
Where they disagree, the defect belongs to whichever one is
semantically incoherent.

## Affix table

### A1. Three affixes' degree lists are shifted by one row

`data/affixes.tsv` rows 487-489 hold the wrong meanings:

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

This one is live, not historical: `data/data.json` was built from the
spreadsheet, so the shipped lexicon glosses MET as empty, GPJ as
metonymy and ENS as thiocyanate chemistry.

### A2. Two different affixes share the Cs cluster -ḑg-

- **MDI**, Modification, D1: "entity used to stop X", "entity used to
  lessen/mitigate X", …
- **S07**, Position/state intertwined in 3D volume, D1: "interior
  movement through integrated 3D midst", …

Both sources carry both entries, so this is in the language, not in a
mirror of it. The two are unrelated in meaning and identical in
surface form, and both are ordinary Vx-Cs affixes occupying the same
slots — nothing in a formative can distinguish them. Any lookup keyed
on Cs has to pick one and lose the other.

### A3. SPT has two C_S forms and no rule for choosing between them

§6.0 introduces the affix as "**SPT -- Specified Points in Calendrical
Time** (form: -**rw**/-**ry**)" — two consonant forms, one entry. The
spreadsheet accordingly carries two SPT rows, at `rw` and at `ry`, with
identical degree lists. `grammar_reference/affixes_reference.md` has
only `ry` and assigns nothing to `rw`, so it is the reference document
that is incomplete here, not the spreadsheet that is duplicated.

What no source supplies is a rule. `-rw-`/`-ry-` is stated once and
never mentioned again; §6.0's degrees, examples and prose never
distinguish the two. The case-accessor affixes of §3.9.2 split their
increments the same way and do say which is which — w-forms for cases
1-36, y-forms for 37-68 — so the machinery for stating such a rule
exists and was not used here. Compare G7, where the Pattern-2 Mood FAC
value is likewise written "w/y" with nothing to choose by.

### A4. ANG's Type cell holds a spilled degree list

The spreadsheet's Type column for ANG (`dg`, Angular Measurement) reads:

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

### A5. Six functional-group affixes have no meanings at all

GPB (`sļ`), GPC (`šḑ`), GPD (`šļ`), GPE (`zḑ`), GPF (`zļ`) and GPG
(`žḑ`) carry a Cs, an abbreviation and a description, and nine empty
degree cells — in both sources. GPA, GPH and GPJ around them are fully
populated. Fifty-four degrees of the chemistry vocabulary were reserved
and never written.

(The series also skips GPI, presumably to keep I and J apart.)

### A6. XCL is absent from the spreadsheet

XCL (`çx`, External Standard for Comparison for Use with Levels) has a
full entry in the reference document and no row in the spreadsheet. It
had to be restored by hand.

## Lexicon

### L1. Five root clusters carry two unrelated meanings

| Cr | First meaning | Second meaning |
|----|---------------|----------------|
| ksmy | oven † | jagged line |
| nļt | groin undergarment | cicadomorphic bug |
| lzbḑ | psychodomorph (drain fly) | tabanid fly |
| rţnw | vitaceae 2 | rosoideae 7 |
| cfw | magnoliaceae | myristicaceae |

Only `ksmy` has a marker resolving the conflict: its first entry is
daggered, and the dagger is used throughout the spreadsheet for retired
words. The other four are two live homonyms apiece with no way to tell
them apart.

### L2. Two roots are phonotactically illegal

- `ňkhw` 'tetherball/sleight-of-hand skills contest' — §2.16 bars **ň**
  before **k**.
- `řẓňy` 'siphonapteran (flea) 2' — §2.16 bars **ň** before **y**.

§2.16 gives its reasons in full (n already assimilates to [ŋ] before
velars; \*ňy is indistinguishable from ny), and neither root has a
dagger, so these read as typos rather than as exceptions.

The other rule-violating roots in the lexicon are all retired:
`mps` 'relative clause head †', `mpš` 'framed relation †', `mpm`, `mpn`
and `mpx` violate §2.13, and every one of them is daggered. The rule
and the vocabulary agree everywhere except at L2.

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

### G3. §3.6's two alternate-form rules have their conditions swapped

The Ca table prints two components with a primary and an alternate
form — Extension (`t / d`, `k / g`, `p / b`, `g / gz`, `b / bz`) and
the RPV column of Perspective+Essence (`m / h` for N, `n / ç` for A) —
and two rules for choosing between them:

> Use the alternate Extension form when preceded by [C]**t**-, [C]**k**-,
> or [C]**p**-.
>
> Use the alternate Perspective+Essence RPV form (for N and A) when the
> Configuration of the word is UPX.

Composing all 3840 Ca values four ways — the two rules as printed, with
their conditions exchanged, and with both conditions the same — gives:

| Reading | Distinct forms | Colliding | Phonotactically illegal |
|---------|---------------|-----------|------------------------|
| As printed (Ext ← t/k/p, RPV ← UPX) | 3767 | 73 | 100 |
| **Conditions exchanged** | **3840** | **0** | **4** |
| Both on t/k/p | 3744 | 96 | 100 |
| Both on UPX | 3839 | 1 | 12 |

Only the exchanged reading is a bijection. As printed, 73 clusters
carry two or more meanings and the Ca complex stops being decodable:
`rt` is both UPX/COA/M/PRX/NRM and MSS/COA/M/DEL/NRM, `py` is both
UPX/CSL/A/ATV/NRM and MSF/CSL/A/DEL/NRM, and so on for 71 more.

The exchanged reading is also the one the forms themselves argue for.
UPX contributes no Configuration consonant, so a UPX Extension would
sit alone where a Configuration consonant would otherwise be — `t` for
UPX/PRX against `t` for MSS/DEL. The voiced alternate `d` is what keeps
those apart, which is exactly the collision the printed reading
produces. Likewise the RPV alternates `h` and `ç` avoid a stop plus
nasal, which is what "preceded by [C]t-, [C]k-, [C]p-" describes.

(The four remaining illegal forms under the exchanged reading are the
single `ţḑ` configuration of G2 above, with its three Affiliation
prefixes.)

### G4. The Ca substitution list is missing a character and a rule

The §3.6 substitution list reads, in its third column:

```
rr → ns
ř  → nš
řr → ňs
```

Applied literally, `ř → nš` rewrites every ř in every Ca — including
the VAR Affiliation prefix and the G/RPV Perspective suffix, neither of
which is part of a cluster the rule could be about. The result is 385
phonotactically illegal Ca forms (`cḑš`, `fḑš`, `kḑš` … all violating
§2.23) and 48 that contain a geminate. The entry has to be **rř → nš**:
that completes the r/ř matrix the other two entries start, and it is
the only reading under which the list works at all.

Even repaired, the matrix is missing its fourth cell. `rr`, `rř` and
`řr` are given; `řř` is not. It arises — UPX/VAR/G/DEL/RPV composes the
VAR prefix `ř` with the G/RPV suffix `ř` — and with no rule to rewrite
it, that configuration's bare Ca is the geminate `řř`, which §3.6.1
reserves for marking the end of Slot V. The pattern of the other three
entries (r→n/ř→ň in first position, r→s/ř→š in second) supplies the
missing value `ňš` unambiguously, but the document never states it.

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

### G13. The documents disagree on how to write ẓ and ḑ

§1.1's phoneme chart gives the affricates as **c ẓ č j**, and §1.3 lists
the sanctioned alternate spellings: ţ may be written ṭ or ŧ, ḑ as ḍ or
đ, ň as ṇ or ŋ, ř as ṛ or ṙ, ļ as ł or ḷ. **ẓ is not on that list** — it
has no sanctioned variant. The documents use one anyway:

| | ẓ (U+1E93, dot below) | ż (U+017C, dot above) |
|---|---|---|
| morphology.md | 8 | 1 |
| phonotactics.md | 0 | 23 |
| affixes_reference.md | 31 | 0 |
| data.json | 162 | 0 |

Every ẓ in the phonotactics document is written ż, including in §2.2's
roster of the sibilants and in §2.5's list of prohibited conjuncts —
the places a reader goes to learn which characters the language has.
morphology.md keeps ẓ throughout except once, in §12.2.1's table of
core script characters.

ḑ has the same problem in miniature, and there the variant is at least
sanctioned: the §4.6 referential table gives the mi/DETRIMENTAL form as
**đ**, the only place in the document that exercises §1.3's alternate,
and a reader working through the tables meets a character that appears
nowhere else and is absent from §1.1 as printed.

Either way a machine reader checking the phonotactic rules against the
phoneme inventory sees an unknown codepoint twenty-three times.

### G14. The V_K table marks two of its eleven forms wrongly

§3.9.3 states that "the V_K affix for verbs uses the same vowel-forms
as the V_C case affix", and V_K is by definition the ultimate-stressed
Slot IX, so every V_K form should carry a stress diacritic placed by
§1.3.1: an undiacriticked vowel takes the acute, a vowel with dieresis
takes the circumflex instead.

Nine of the eleven follow that exactly — á, é, í, ô, ó, û, ú for the
Validations and ái, áu, éi, éu, óu, ói, íu, úi for the non-Assertive
Illocutions, each marked on the prominent first member. Two do not:

- **REC** is printed **à**, a grave on plain `a`. The Validations run
  down the standard vowel sequence, so REC is form 2, `ä` — the V_C
  table gives `ä` for case 2, and IMA `ö → ô` and ITU `ü → û` in the
  same column confirm the series and the convention. REC should be
  **â**. As printed it is the only grave accent in the table, and
  §1.3.1 reserves the grave for an *unstressed* -i- (or -u-) as the
  first member of a vocalic conjunct — never for a stressed vowel and
  never on `a`.
- **USP** is printed **ëi**, with no mark at all, where §1.3.1 would
  give **êi**. Unmarked means penultimate stress, which contradicts
  V_K's own definition.

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

### G17. §3.9.2 requires fourteen C_S increments and publishes eight

The prose is explicit about the inventory:

> There are two separate C_S increments for each of the seven types of
> affix (Types-1, -2, and -3 Case-Accessor, Types-1, -2, and -3 Inverse
> Case-Accessor, and Case-Stacking Affix), the first C_S increment
> being used for Cases 1 through 36, while the second C_S increment is
> used for Cases 37 through 68.

Seven types times two increments is fourteen. The table below it
supplies four pairs — **sw**/**sy**, **čw**/**čy**, **šw**/**šy** and
**lw**/**ly** — and those eight forms are every occurrence of an
accessor increment anywhere in the document. Six are missing, and with
them three of the seven affix types cannot be written at all.

The table is inconsistent with itself as well: its second header row
names seven columns (Type-1/2/3 Case-Accessor, Type-1/2/3 Inverse
Case-Accessor, Case-Stacking), while its third header row lays out only
four `V_X = Series No. | C_S =` column pairs, which is what the data
rows fill.

What is published is otherwise sound. The V_X carries the case-group in
its series and the case within the group in its form — four series by
nine forms for cases 1-36, by eight for cases 37-68 with vowel-tier 8
unused, giving exactly 36 and 32. And none of the eight increments
collides with an ordinary affix: `sw`, `čw`, `šw`, `lw`, `sy`, `čy`,
`šy` and `ly` are all absent from the 527-affix table, though twenty
other two-consonant forms ending in -w or -y are taken.

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

### G19. §8's three sections disagree about how numbers are built

**§8.1 against §8.2.** §8.1 says "The numbers from zero to 100 are
considered autonomous units represented by **single stems**". §8.2 then
gives fifteen number roots — 0-10, 100, 100², 100⁴, 100⁸ — and builds
everything between 11 and 99 as a root plus the TNX affix (-rs), whose
nine degrees are +10 through +90. So 89 of the 101 numbers §8.1 calls
single stems are a stem plus an affix, and §8.3's own examples show it:
*wallärsa* is `ll` (one) with TNX degree 2 (+20), i.e. 21.

**§8.2 against itself, two lines apart.** Its opening sentence reads
"The roots for numbers **1 to 99** are based on roots for **0 through
10**, to which the nine degrees of the TNX affix (-rs) are added". The
affix's own note says "Used with roots **0-9** to create numbers
**11-99**". Both differences matter. Numbers 1-9 take no affix at all,
and 10 has its own root `-J-`; and admitting root 10 to the
construction would generate 20, 30 … 100 a second way, with the last of
them colliding with the dedicated root for 100, `-GZ-`. Only the affix
note's version works. (Even it leaves one redundancy the document does
not mention: root 0 with TNX degree 1 is "0 plus 10", which is 10
again.)

**§8.3's coordinative affix does not match its label, and appears in
none of its examples.** The rule reads "plus the coordinative affix
**-iň** (COO/1)". COO is `ň` and the Type-1 V_X series runs a, ä, e, i,
ëi, ö, o, ü, u — so `-iň` is COO degree **4**, "and in sequence", while
COO/1 would be `-aň`, "and w/shared topic". For enumerating digits the
degree-4 sense is the apt one, which suggests the form is right and the
label wrong, but the document states both.

Either way the affix is never exercised. §8.3 gives four worked
examples and `ň` occurs in none of them — including 4,229 =
*ksalirsa (gzalui) walẓärs*, which is 42 hundreds plus 29 and so is
exactly the case the rule describes: "Single units (1-99) connect with
the coordinative affix when part of hundreds or higher". The single
unit there, *walẓärs*, carries no coordinative affix. `-iň` occurs
once in the entire document, in the sentence that introduces it.

Everything else in §8 checks out. All twenty number roots match the
lexicon (`vr` zero, `ll` one … `čg` ten-quadrillion, plus `cg` `jd`
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

The version history for v1.3 records the change:

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

### G27. §7.0 gives CLG a form that no affix bears and §2.2 prohibits

§7.0 introduces "**CLG -- Cultural or Geo-Demographic Association**
(form: -**dc**)". The affix table has CLG at **ḑc**, and `dc` is
assigned to no affix at all.

The cited form is not merely unassigned, it is unpronounceable: §2.2
says "The dental stops (**t**, **d**) cannot be immediately followed by
any sibilant (s, z, š, ž, c, ż, č, j)", and `c` is a sibilant
affricate. `ḑ` is an interdental fricative, not a dental stop, so `ḑc`
is unaffected — §2.23's bar on ḑ before a sibilant names only the
fricatives s, š, z, ž. A missing cedilla turns a legal cluster into a
prohibited one.

Every other affix form cited in the body checks out against the affix
table: PTN `sv`, CHC `rz`, SEQ `nt`, OGC `dn`, POR `ft`, TNX `rs`,
SIZ `x`, and SPT `rw`/`ry` per A3.

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
occurs.


### G31. The DES bias is printed mřr, which §2.21 forbids; the form is mřř

§4.7's table gives the DESPERATIVE bias as **mřr**. §2.21 is
unambiguous — "The uvular approximant -**ř**- cannot be followed by
-**r**-" — so as printed the form is unpronounceable in any position,
not merely word-initially.

The geminate `mřř` is what the language actually uses. It stands alone
as a word 42 times in the community corpus against a single instance of
`mřr`, `data/data.json` records it as `mřř`, and unlike `mřr` it is
legal: §6.4.1 admits a word-initial #C₁C₂C₂- whenever #C₁C₂- is
permitted, and §3.2.8 permits a word-initial nasal before the
approximant ř.

A dropped diacritic, then — but one that turns a legal word into a
prohibited one, the same shape of error as §7.0's `dc` for `ḑc` in G27.

### G32. §12.2 counts 28 core script characters and tabulates 30

§12.2.1 introduces the core secondary characters with "The **28** core
forms each have a 'top' and 'bottom' end that take extensions", and
§12.2.2 opens "For each of the **28** consonants, there are three
extension orientations". The table between the two sentences lists
thirty:

> p b f v t d ţ ḑ k g s z x š ž c ż č j ç h l r ļ ř m n ň w y

That is exactly §1.1's consonant inventory — all thirty letters, with
nothing missing and nothing extra — leaving out only the glottal stop,
which is the one consonant with no secondary character. The table is
right and complete; the count in the prose on either side of it is not.
