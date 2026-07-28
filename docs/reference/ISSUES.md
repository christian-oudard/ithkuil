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
   `ithkuil.place/4/archive/latest/` supplies the affix, phonotactics,
   lexicon and script documents — five in all. Local copies live in
   `$XDG_DATA_HOME/ithkuil/reference/`, outside the repo.
2. **The Collaborative Ithkuil IV Roots and Affixes Spreadsheet**
   (Google Sheets `1JdaG1PaSQJRE2LpILvdzthbzz1k_a0VT86XSXouwGy8`),
   which `tools/sync_lexicon.py` mirrors. *Fetched live and checked.*
3. **`docs/reference/*.md`** — our markdown transcriptions of layer 1.
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
| `morphology.md` §3.5.0.1-2 | V_XC_S Affixes for New Ithkuil v1.1, merged in | transcription under our own section numbers |
| `morphology.md` §8 | §6.4 of Lexicon for New Ithkuil, v1.0 (Feb. 9, 2023) | transcription under our own chapter number |
| `phonotactics.md` | Phonotactic Rules for the Ithkuil Successor Language, v0.5.4, 24 pp. | transcription |
| `affixes_reference.md` | `data/data.json`, reconciled against V_XC_S Affixes for New Ithkuil v1.1 | generated content |
| `source_versions.md` | the version histories of the above | transcription |

Every chapter of `morphology.md` now has a layer-1 source. §8 did not
until the lexicon document was obtained: the grammar document ends at
Chapter 7 and says nothing about numbers, so §8 had been condensed from
Chapter 13 of the 2011 grammar. Quijada does cover numbers, in §6.4 of
the lexicon, and §8 is now a transcription of that. Only the chapter
number is ours.

The change is not cosmetic. The 2011 material carried spoken-number
rules, worked examples and a Stem/Specification summary that the V4
document does not repeat — it says only that numbers from 101 "are
formed as in Ithkuil-2011 using the COMITATIVE case and the COO affix",
and refers the construction back there. `numbers` still implements it
and now cites ch. 13 for the parts that only ch. 13 states.

§8 carries §6.4 in full: the two root tables, the ten Stem and
Specification tables (-Z- as the pattern for 2 and up, -VR-, -LL-, and
the seven operation and number-theory roots), and the notes on -TVY-
concatenation and the OAU affix.

**Audited:** every table in `morphology.md` §3 has now been checked
against the PDF — the 68 cases positionally and against the vowel-form
table, the 32 V_N and 36 aspect cells, V_V, V_R, the 17 V_K values, and
every section number. One transcription defect came out of it (G38),
one misplaced heading (§3.9.3.3 had lost its title and its table), and
one code defect (`parse` read the NAV alternate as `i'ë` where the
vowel-form table gives `i'ä`). `phonotactics.md` §§8-11 summarise
Quijada's combinatorial tables — 679, 5183, 15034 and 12271 permissible
forms — rather than reproducing them; the totals match the document but
the forms themselves are not here.

### How much of this list to believe

Thirteen entries have been settled, and eleven of those were defects in
our own transcription rather than in Quijada. That is the base rate to
apply to everything below.

The ones that fell hardest looked structural, not clerical. G3 argued
that the C_A table's two alternate-form conditions were swapped; the
PDF binds them with superscript footnote markers our markdown had
dropped, and the binding is the one the 3840-value bijection test had
already identified as the only workable reading. G16 argued that two
affixes' semantics contradicted their declared gradient type; against
Quijada's real definitions both fit exactly, and the mismatch was in a
paraphrase of his wording that we had written ourselves.

**Verified against the source:** the Affix table and Lexicon sections,
and G33, G35, G36, G39. **Not yet:** G2, G5-G13, G15, G18, G20-G26,
G28, G30, G37 and the Corpus section. Those have been read against our
markdown, not against the PDF, so each is a claim about a document that
has been wrong eleven times.

## Settled

Removed from the list below; the reasoning is in the commit that
settled each. Kept as a ledger because the proportion is the useful part. Eleven of
the thirteen were defects in our own transcription, not in Quijada;
one was a transposition in the community spreadsheet; one was a real
gap in the published data that turned out to be recoverable. None was
a defect in the language.

| | Disposition |
|---|---|
| A2 | the ḑg collision is a spreadsheet transposition |
| A5 | fifty-four functional-group degrees recovered |
| G1 | there is no §2.24, and this entry is the proof |
| G3 | the conditions are footnotes, and our markdown lost them |
| G4 | the substitution list is complete in the source |
| G14 | the V_K diacritics are correct in the source |
| G17 | the PDF publishes all fourteen increments |
| G19 | §8 now transcribes Quijada, not the 2011 grammar |
| G27 | CLG is -ḑc in the source |
| G31 | the DES bias is mřř in the source |
| G32 | the script document tabulates 28, as it says |
| G34 | Our markdown lost three of the seven case-accessor affixes |
| G38 | §3.5.0's gradient-type descriptions were ours, and four were wrong |

## Affix table
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

## Grammar

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

### G16. The gradient-type shape rules are a mnemonic, not a function

*Rewritten. The earlier version of this entry quoted our paraphrase of
the affix document rather than the document, and two of its three
counterexamples were artifacts of that paraphrase. See G38.*

Quijada does not claim the shape rules decide the type. He says each
type "has a particular phonological signature associated with the
affix's consonant-form **as a mnemonic aid in learning the affixes**".
Read that way the section is sound, and this entry is only about how
far the mnemonic actually reaches.

Classifying all 527 affixes by the seven shape descriptions and
comparing against their declared types, 512 of the 526 typed affixes
sit in a shape-class that maps to more than one type. Three things
have to be supplied before the rules resolve:

**Type 0 is not a shape class.** Its own text gives the game away:
"No gradient pattern. The nine degrees of the affix do not manifest
any particular hierarchical semantic gradient." That is a semantic
property, and it is assigned to 221 affixes spread across *every*
shape class — 27 that begin with r/ř (A1's shape), 9 ending in -m or
-x (D2's), 4 containing ç (B's), 3 ending in -ř (C's), 2 ending in -h
(A2's), and 33 matching no shape rule at all. Its stated shape covers
only 171 of the 221. Setting Type 0 aside as semantically assigned
leaves 271 affixes for the shape rules to determine.

**Type 0's last clause overreaches.** It reads "or a bi-consonantal
form ending in a sibilant", which catches sibilant + sibilant pairs and
so claims 14 affixes that are declared D1 — `šč`, `cj`, `žč`, `sc`,
`sj`, `zj` and their like. The preceding clauses already cover a
sibilant + stop and a sibilant + fricative, so the last is evidently
meant for a *non*-sibilant followed by a sibilant. Read that way, the
14 stop being misclaimed.

**The rules need a priority order, and none is given.** Thirty-seven
C_S forms match two or more shape rules: `rř` is both A1 and C, `rx`
and `rm` are both A1 and D2, `rç` is both A1 and B, `çx` is both B and
D2, and 16 forms are both Type 0 and A1. Only A2 states a precedence
("except -rh and -řh, which are Type A1"). The declared types imply
C > A1 > B > D2 consistently, but the document never says so.

With those three supplied, the shape rules land 268 of the remaining
271. The three exceptions are all shape, not semantics — each affix's
nine degrees fit its declared type exactly:

| Affix | C_S | Declared | Shape says | Semantics |
|-------|-----|----------|-----------|-----------|
| AUT | `pč` | B | Type 0 — no ç | fits B |
| COO | `ň` | C | no rule matches | fits C |
| NEG | `r` | D1 | A1 — begins with r | fits D1 |

COO is the clearest. Type C wants Degree 1 at one extreme running to
the other by Degree 4, Degree 5 neutral or meta-level, and Degrees 6
through 9 cycling back through the same values under an orthogonal
sub-parameter. COO gives shared topic, shared morphology, shared
participant, in sequence (1-4); plain "and" (5); then at the same time,
at the same time with shared participant, with shared morphology, with
shared topic (6-9) — the same three values again, mirrored, under the
orthogonal parameter "at the same time". Only `ň` not ending in -ř is
out of place.

AUT's nine degrees group three by three as Type B requires: grounds of
entitlement (privilege, natural right, statute), grounds of exchange
(custom, favour, threat), grounds of office (own rank, social class,
governing entity). Only `pč` containing no ç is out of place.

NEG runs from relative negation (1-4) through "neither the preceding
nor X" (5) to absolute negation (6-9), which is D1's trivalent "-1 to
0 to +1" with a midpoint, not A1's bivalent extreme-to-extreme. `r` is
the one affix where the A1 shape rule points at the wrong type.

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

The version history for v1.3 (see `source_versions.md`) records the change:

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

### G29. The asterisk does not distinguish what it says it does

*The framing of this entry was corrected: the marker belongs to the
abbreviation, not to Type 0. Our §3.5.0.1 had it as a type marker
("Some Type-0 affixes are marked 0*"), which is why the entry
originally asked what "0*" could mean and why a stray "D1*" looked
anomalous. Quijada writes: "Those affixes whose three-letter
abbreviations are followed by an asterisk (e.g., DNG*) have an
associated C_R root." Any type can carry it. See G38.*

The claim itself still does not hold. Read as an existence claim it is
uninformative, because it is true of almost every affix:

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

The marker **D1\*** on NEW (`sp`, Newness/Revision) is not itself an
anomaly: Quijada attaches the asterisk to the abbreviation, so a D1
affix may carry one, and NEW's root at `sp` is "degree of newness".
It only looked anomalous while our §3.5.0.1 tied the star to Type 0.

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

### G37. Two prohibited-conjunct rules in our markdown have no source

*A transcription defect, in layer 3.* `phonotactics.md` carried
twenty-four numbered rules in §2. Quijada's document has twenty-two:
it ends at 2.22, "the semiconsonants -w- and -y- can only appear as
the last member of the conjunct". Our 2.23 and 2.24 were:

> **2.23.** The following combinations are considered phonetically
> awkward and are not permitted: -ḑs-, -ḑš-, -ḑz-, -ḑž-, and -nň-.
>
> **2.24.** Because the consonant forms -ç- and -hl- (pronounceable as
> -ļ-) figure so prominently in the language in terms of morphology,
> to avoid any confusion the geminated forms -çç- and -ļļ- are not
> permitted.

Neither appears in phonotaxis v0.5.4, which is the newest version
published — the ithkuil.place archive lists v0.3, v0.4, v0.5.0, v0.5.2
and v0.5.4, and nothing since 2021. Neither appears in the grammar,
affix or script documents, nor in the 2011 phonology chapter. The
strings `ḑs`, `nň`, `çç`, `ļļ` and the phrases "phonetically awkward"
and "so prominently" are absent from all of them.

The content of the two rules stands differently:

- **2.23 is corroborated.** None of `ḑs`, `ḑš`, `ḑz`, `ḑž` or `nň`
  occurs in any root or affix C_S in the lexicon.
- **2.24 is contradicted.** Twenty-four lexicon roots contain `çç` or
  `ļļ` — `ççk` "maximum", `pļļ` "humor", `ļļtļ` "heaven/paradise" and
  so on. All are community coinages rather than Quijada's, so they do
  not settle whether the rule exists, but nothing enforces it either.

Both are removed from the transcription. What our code does about them
is a question about our code, and is recorded where the repo keeps
those: as a skipped test beside the constraint, in `validation`.

The same import summarised §§8-11 rather than transcribing them, which
is defensible — they are combinatorial tables of 679, 5183, 15034 and
12271 forms. But §8's total was given as 682 against the document's
679, printed twice, and a clause was added to Quijada's sentence about
-ç- and -ļ- saying their geminates "are not permitted as roots", which
is the same unsourced claim as 2.24. Both corrected.

### G39. The lexicon promises six extra number roots and prints five

§6.4.1 gives the basic roots for 0 through 10 and the powers of 100,
then introduces a second set:

> The following **six** number roots are used when needed to designate
> numbers beyond ten when needed for counting and mathematical
> operations involving non-decimal number bases up to base-16.

The table beneath it holds five: 11 `-CG-`, 12 `-JD-`, 13 `-ĻJ-`,
14 `-BC-`, 15 `-ŢẒ-`. Nothing in the lexicon supplies a sixth, and no
sixth is needed — the stated purpose is bases up to 16, and a base-16
system needs digits for 0 through 15, which 0-10 plus 11-15 already
covers. A root for 16 would be the first digit of the *next* place, the
same role `-GZ-` plays for base-100.

The same sentence with the same off-by-one appears in Chapter 13 of the
2011 grammar, so the count has been wrong across both versions rather
than introduced in the V4 rewrite. The word is wrong, not the table.

### G40. §4.6 does not say which spelling of a category affix to use

§4.6 marks the Agglomerative, Nomic and Abstract categories on a
referential by adding an affix "immediately preceding or following one
of the affixes above (as phonotactically permissible)", and gives two
spellings for each:

| Category | Forms | Written |
|---|---|---|
| Agglomerative | ļ, tļ | `-ļ-` / `-tļ-` |
| Nomic | ç, x | `-ç-` / `-x-` |
| Abstract | w, y | `-w` / `-y` |

Two things are left open. Which of the two forms, and which side.

The phonotactics settle most cases on their own. Of `lça`, `lxa`,
`çla` and `xla` — the Nomic category on the 1m referent — only the two
prefixed forms are clusters a word may open with, so the affix has to
precede. On the 2m referent it is the other way round: `çsa` and `sça`
break the rules about ç beside a sibilant, and `xsa` breaks §2.17, so
only the suffixed `sxa` survives. This is presumably what "as
phonotactically permissible" is doing.

It does not always narrow to one. `çla` and `xla` are both legal, as
are `tļma`, `mtļa` and `ļma`, and the Abstract `lwa` and `lya`. The
section offers no way to choose, and Ithkuil generally avoids free
variation, so something is probably missing rather than genuinely
free.

The hyphens are the one hint the source gives. Agglomerative and Nomic
are written with a hyphen on both sides, its notation elsewhere for an
affix that may attach either way; Abstract is written `-w` / `-y`,
with a leading hyphen only, which reads as suffix-only. §4.6.5 repeats
Abstract in that form ("cannot add the ABSTRACT Perspective increments
-**w** or -**y**"), so it is consistent rather than a typo. We take it
as meaningful.

Where more than one spelling still survives, our renderer takes the
first the section lists. That ranking is ours, not Quijada's: the
canonicalization heuristics in SPEC.md rank optional shortenings and
have nothing to say here, the candidates being identical in syllable
count, glottal count and length. Recorded so the choice is not later
mistaken for something the source decided.

**Related:** §4.6.4 bars all three categories from a specialized
personal-reference root, where the Slot VI Perspective carries the
same distinctions instead. That much the source is explicit about.

### G41. §2's prohibited conjuncts rule out three of Quijada's own examples

Two §2 rules bar conjuncts that the morphology document then uses in
worked examples of its own.

**2.2** bars a dental stop before any sibilant, "as these would be
homophonic with the various sibilant affixes ... or their geminated
forms". **2.9** bars a sibilant affricate before a sibilant fricative,
"e.g., \***čs**, \***cz**, \***żz**, \***čž**, \***żs**, \***js**,
\***jz**, \***jš**".

The examples that break them, all in `morphology.md`:

| Word | Line | Conjunct | Rule |
|---|---|---|---|
| **Tladatra cskava.** *Disease is rampant there.* | 1352 | `cs` | 2.9 |
| **Mala welu wiosadca espanya.** *The child is speaking (in) Spanish.* | 1355 | `dc` | 2.2 |
| **Adcsuleuha.** 'jump'-DYN-RCT-OBS | 1515 | `dcs` | 2.2 |

Two roots are involved, `-csk-` and `-dcs-`, and both are in the
lexicon as well, so the disagreement is not confined to the prose.

The rules are transcribed correctly and our implementation applies
them as written; this is the source disagreeing with itself, not a
defect in the transcription or the code. It is also narrow. Of the 105
worked-example words the corpus tests cover, these are the only
failures, and of 5946 lexicon roots only those two break a §2 pair
rule.

Which side is wrong is open. 2.2's own justification argues against
the examples — `dc` really would be hard to tell from `cc` — which
suggests the roots are the error. Against that, they are Quijada's own
roots used in his own glossed sentences, in three separate places, and
a typo repeated three times across two documents is a stretch.

Nothing has been changed either way. `validate` and `parse` reject all
three words today. Recorded because fixing an unrelated bug made it
visible: `parse` had been skipping validation for any capitalized
word, so `Adcsuleuha` used to pass while its two lower-case siblings
failed, which read like an inconsistency in the parser rather than a
question about the grammar.

**Related:** G37 records two prohibited-conjunct rules in the same
section that have no source at all.
