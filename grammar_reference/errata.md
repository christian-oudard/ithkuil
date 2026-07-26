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

### A3. -rw- is a spurious duplicate of SPT

The spreadsheet lists SPT (Specified Points in Calendrical Time) twice,
at `rw` and at `ry`, with byte-identical degree lists. The reference
document has SPT at `ry` only and assigns nothing to `rw`. The `rw` row
looks like a copy that was never given its own affix.

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
Taking the word-initial inventory at its word, 35 of the 61 forms are
not licensed:

- **§3.2.9** — "Word-initial liquids l- and r- may be followed by the
  semiconsonants -w or -y", and by nothing else in §3: ACC `lf`,
  ADS `lļ`, SAT `lţ`, ANP `lst`, FOR `lzp`, ISP `lçp`, SGS `ltç`,
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
- **§2.21** — "-ř- cannot be followed by -r-", in any position:
  DES `mřr`.

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

### G13. The referential table switches romanization mid-document

§1.3 sanctions alternate spellings — **ḑ** may be written **ḍ** or
**đ** — and the document uses **ḑ** everywhere it appears: the §1.1
phoneme chart, the §3.6 Ca substitutions, the affix tables, the
lexicon. The one exception is the §4.6 referential table, which gives
the mi/DETRIMENTAL form as **đ**.

Nothing is wrong with the form; it is the only place the alternate is
exercised, and a reader working through the tables meets a character
that appears nowhere else and is absent from the §1.1 inventory as
printed.

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
