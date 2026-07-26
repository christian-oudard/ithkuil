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

### G3. The Pattern-2 Mood FAC value is written "w/y" with no selection rule

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

### G4. Most bias adjuncts are not permissible words

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

### G5. §4.7's stated design principle contradicts its own table

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

### G6. §1.2.1's vowel inventory does not cover the conjuncts the morphology uses

§1.2.1 gives ten permissible diphthongs — ai, ei, ëi, oi, ui, au, eu,
ëu, ou, iu — and describes every other two-vowel sequence as a
"disyllabic conjunct". But the morphology's own vowel-form tables use
**ae**, **ea**, **üo** and **üö** as form-0 values, and §4.6.3 calls
**üo** a "word-initial diphthong" outright. Under §1.2.1 it is not a
diphthong, and the tables never say how many syllables these forms
carry — which matters, because syllable count is what selects the
formative's Relation in Slot X.
