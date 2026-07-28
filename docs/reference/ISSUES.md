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

### G12. §1.2.1's vowel inventory does not cover the conjuncts the morphology uses

§1.2.1 gives ten permissible diphthongs — ai, ei, ëi, oi, ui, au, eu,
ëu, ou, iu — and describes every other two-vowel sequence as a
"disyllabic conjunct". But the morphology's own vowel-form tables use
**ae**, **ea**, **üo** and **üö** as form-0 values, and §4.6.3 calls
**üo** a "word-initial diphthong" outright. Under §1.2.1 it is not a
diphthong, and the tables never say how many syllables these forms
carry — which matters, because syllable count is what selects the
formative's Relation in Slot X.

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
