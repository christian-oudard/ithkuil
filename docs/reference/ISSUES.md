# Issues in the V4 Source Material

Defects in the published grammar, the affix tables and the community
lexicon — not in this repository's code.

**This file is a worklist, not a record.** An entry sits here until we
decide what to do about it. That decision is written up in `ERRATA.md`,
which is permanent, and the entry here goes away. When this list
reaches zero it will be deleted and `ERRATA.md` will be the only one
left.

So: a defect with no decision is here. A defect with a decision is in
`ERRATA.md`. `BUGS.md` is separate again, and is about our code failing
to do what we intend rather than about the language.

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
| `READING.md` | the version histories of the above | transcription |

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

**Audited.** `morphology.md` has now been checked against the PDF in
three passes, each catching what the one before could not see:

- Every table in §3, positionally and against the vowel-form table: the
  68 cases, the 32 V_N and 36 aspect cells, V_V, V_R, the 17 V_K values
  and every section number. It produced G38, a misplaced heading
  (§3.9.3.3 had lost its title and its table) and a code defect
  (`parse` read the NAV alternate as `i'ë` where the vowel-form table
  gives `i'ä`).
- Every Ithkuil form carrying a diacritic, both directions. 63 of 490
  were wrong, in four ways: diacritics dropped, letters transliterated
  to ASCII, glottal stops dropped, and §7's name tables reflowed out of
  the source's grid and misaligned. `TestMorphologyFormsMatchSource`
  keeps this one from coming back.
- Every sentence of running prose, matched against its nearest
  counterpart. It produced nine more, of which one changed a meaning
  (§4.5.3 had the Carrier and Quotative adjuncts the wrong way round)
  and two were letters that only a rendered page could settle (§1.2.2's
  hr and hn, §1.3's ň and ř alternates).

`phonotactics.md` §§8-11 now carry Quijada's four combinatorial tables
in full rather than summarising them, and `TestSection8GridMatchesRules`
regenerates §8 from our reading of §§1-7. G47 and G48 came out of that.
Its prose has had the sentence pass too, which found three dropped
example forms, one of which `phonotactics.go` had dropped with it.

The two remaining files have had the form check but not the sentence
pass. `morphology.md` §8 agrees with §6.4.1 of the lexicon document,
both directions, on every form either carries. `affixes_reference.md`
agrees with the affix document except for A8.

### How much of this list to believe

Twenty-six entries have now been settled, and twelve of those were
defects in our own transcription rather than in Quijada. That is the
base rate to apply to anything new: the first hypothesis for a defect
found here is that we mis-transcribed it.

The ones that fell hardest looked structural, not clerical. G3 argued
that the C_A table's two alternate-form conditions were swapped; the
PDF binds them with superscript footnote markers our markdown had
dropped, and the binding is the one the 3840-value bijection test had
already identified as the only workable reading. G16 argued that two
affixes' semantics contradicted their declared gradient type; against
Quijada's real definitions both fit exactly, and the mismatch was in a
paraphrase of his wording that we had written ourselves.

G5 and G6 went without a writeup, being our confusion rather than
Quijada's defect. Both read §3.6.1's nine gemination rules as a
dispatch table every C_A must match, and counted the forms none of them
names as unreachable. The rules are a default plus exceptions:
geminating a cluster means doubling its initial consonant, and the nine
say where that does not hold. Every C_A value has a geminate, all legal
and all distinct, which `allomorph/geminate_test.go` now checks and
explains so the reading is not lost a third time.

A rendered page beats the text extraction whenever a single letter is
the point. `pdftotext` reported the SAT bias as ļţ where our markdown
had lţ, and reported §1.3's ň alternates as `n͕ or ṇ`; both needed
`pdftoppm` at 600 dpi to settle, and in both cases a low-resolution
render was also wrong, losing a cedilla to antialiasing.

**Verified against the source:** all of `morphology.md` and
`phonotactics.md`, and the Affix table and Lexicon sections.
**Not yet:** the corpus, and the affix and lexicon documents beyond the
places an entry has already reached into them.

## Affix table

*Empty.*

### A9. SPT names two affixes, and both reached the spreadsheet

The spreadsheet gives SPT, "Specified Points in Calendrical Time", to
two C_S forms, `-rw` and `-ry`, with identical nine-degree lists
(seconds, minutes, hours, weekday, day, week of month, month, year,
century). Neither row says what separates them.

A8 is the same defect with the opposite outcome. There the duplicate
name lost one of its affixes on the way into the spreadsheet; here both
arrived, which is worse, because both are reachable. A gloss carries an
abbreviation and a degree and nothing else, so `SPT/3` names two words
and reads back as one:

```
mlalerwa  ->  ml-SPT/3  ->  mlalerwa
mlalerya  ->  ml-SPT/3  ->  mlalerwa
```

No name reaches `-ry`, and which of the two a form belongs to is not
recoverable from a gloss. The two artifacts built from this row lose
opposite halves: the glosser resolves SPT to the alphabetically first
cluster and drops `-ry`, while `affixes_reference.md` prints `-ry` and
drops `-rw`, printing CTC `-ţd` twice in its place. That last part is
ours and belongs in `BUGS.md`; the ambiguity it is downstream of is
not.

**The affix document has now been checked, and it names them the
same.** The entry is printed once, with both forms on the one heading
and a single nine-degree list under it:

```
-rw/-ry   SPT Specified Points in Calendrical Time (used with Stem 3
              of the number roots to render, e.g., 'the third day of
              the week (i.e., Wednesday)' or '20th of May', etc.)
1  second(s) of the minute        6  week of the month
2  minute(s) of the hour          7  month of the year
3  hour of the day                8  year
4  day of the week                9  century
5  day of the month
```

So the spreadsheet did not duplicate anything. It split one affix's two
C_S forms into two rows, because a row there is keyed by C_S and cannot
hold two. The duplicate abbreviation is an artifact of that shape, and
A8 is not the precedent after all: there one of two distinct affixes was
lost, here one affix acquired a second row.

That leaves the harder question, which is Quijada's. **This is the only
affix in 141 pages written with two C_S forms** — no other heading in
the document carries a `/`. Two clusters spell one meaning, and nothing
anywhere says what chooses between them.

The neighbours make the vacancy conspicuous rather than explaining it.
The same table splits its other calendrical families across exactly
this pair of glides, by span:

```
-rkw CYC Cyclic Recurrence          -rky CYL Cyclic Recurrence [Long-Term]
-rţw ITE Iterations Per Time-Period -rţy ILT Iterations Per Long-Term
```

`w` is the short-term member and `y` the long-term one, twice over, and
SPT's own nine degrees run from seconds to centuries, which is the span
those pairs divide between them. So `-rw`/`-ry` sits in the one slot
shaped for a short/long split, and receives one undivided list.

Whether that is a compression Quijada intended or a split he began and
did not finish is not decidable from the document. What is decidable is
that the spreadsheet is faithful to it and no sync will repair this.

**Options.** The first bullet is now answered; the rest stand.

- ~~Check the affix document first.~~ Done: it names them the same, so
  the defect is Quijada's and not the spreadsheet's.
- **Leave both rows as synced.** `SPT/3` stays ambiguous and `-ry`
  stays unreachable by name. Nothing is invented and nothing is fixed.
  Defensible in a way it was not before: the two rows now faithfully
  record that two clusters spell one affix.
- **Collapse to one row carrying both clusters.** Matches the document
  exactly, and makes `SPT/3` unambiguous by making it mean both. Needs
  a lexicon entry to hold two C_S values, which no other affix needs,
  and the store's C_S key would have to admit it.
- **Gloss the unnameable one by its cluster,** `ry/3`. The same
  trade-off G51 weighs for the §3.9.2 affixes, and it should be
  decided once for both rather than twice.
- **Name them apart locally.** A hand-added abbreviation would be
  overwritten by the next sync unless `tools/sync_lexicon.py` carries
  it, which is A8's objection to fixing an affix row in place.
- **Raise it upstream,** where one name was assigned to two rows.

## Grammar

*Empty.*

### G51. Nothing owns the abbreviation namespace a gloss is written in

A gloss is a sequence of abbreviations with no per-token type, as G41
observes: which category a token belongs to is recoverable only from
its shape and its position. Two categories that share three letters are
therefore indistinguishable wherever their positions coincide as well.
The abbreviation space is a shared resource, and nobody maintains it as
one.

Two sources write into it independently, and they do not agree on what
an abbreviation is. The affix tables name 528 affixes, every one of
them exactly three letters. The grammar tables carry 308 values, of
which only 284 are three letters: Perspective is A, G, M and N, the
referents are 1m, 2m, 2p, ma, mi, Mx, pa and pi, the stems are S0
through S3, and the Effects are BEN1-3, BSLF, DET1-3 and DSLF. So the
three-letter convention is the affix spreadsheet's, and the grammar
document does not follow it. Some of the four-letter forms are not
Quijada's either: the community writes the Effects as `1:BEN`, and
BEN1 is this repository's rendering of them.

Across the two sources there is exactly one collision. CNT is both the
CONTINUATIVE Aspect and the affix `-rft` "Degree of Centrality". VRF
was a second, and Quijada removed it himself by renaming the
illocution to VER (G41), which is evidence that he treats a collision
as a defect rather than as something a reader should live with.

The opposite failure is in §3.9.2, and it is not an oversight. Its
seven case-bearing affixes, Case-Accessor Types 1 to 3, Inverse
Case-Accessor Types 1 to 3 and Case-Stacking, have no abbreviations at
all. Quijada's own examples in that section gloss them without one:

```
'event'-PLE/7-INS₃            a Type-3 case-accessor
'event'-PLE/7-Inverse:INS₃    its inverse
```

The case name carries the affix, the Type is a subscript, and
"Inverse" is spelled out. The spreadsheet has no rows for the fourteen
C_S increments either, and could not have: an entry there is a C_S
with nine degrees, whereas an accessor's V_X holds one of the 68 cases
instead. So the naming pass that covered all 528 affixes never reached
these. The community coined CST for the case-stacking affix in its own
usage, and nothing for the other six.

The two failures are one problem seen from either end. A name that
addresses two affixes (A8, A9) and a family that no name addresses are
both what a namespace without a registry produces, and neither is
repairable by writing better code. A gloss is exactly as ambiguous as
the tables its tokens come from.

What this costs here: this repository is the third party writing into
the namespace, and it has not been marking which tokens are its own.
ACC, IAC, ANT and ULT are coinages rather than transcriptions. ACC
collides with Quijada's Accidental bias, which is a defect of ours and
not of the source. ANT and ULT stand where the community writes FRM
and UNF.

**Options, on how to gloss the §3.9.2 affixes.** They are seven, and
the C_S is the only thing in the word that separates a case-accessor
from its inverse, so the gloss cannot leave the family unsaid.

- **Bare case, as Quijada writes it.** Blocked, and worth recording as
  blocked because it is the first thing anyone will propose. `ml-INS`
  already composes to *mlalä*, a formative in the INSTRUMENTAL case.
  His `INS₃` works only because the type subscript and the slot layout
  of his notation tell an affix from a case, and ours has neither.
- **Keep the invented codes.** ACC, IAC, CST. Every token keeps the
  shape every other token has. Adds a fourth writer to an unmanaged
  namespace, and ACC is already a collision.
- **Invent non-colliding codes.** CAC or ACE for the accessor, IAC,
  CST. Same shape, collision gone, still invention.
- **The C_S cluster.** `sw/INS`, `sqw/INS`. No invention, and the
  gloss already writes an affix the lexicon cannot name this way. Puts
  phonetics into a gloss that is phonetics-free everywhere but the
  root, and the root's exemption is earned by being central and
  numerous, which fourteen clusters are not. The cluster also restates
  the Type and the case range, which the rest of the token already
  carries.
- **The source's own words.** `Accessor/INS`, `Inverse/INS`. No
  invention, no phonetics, and word-shaped tokens are already normal
  here: `formative`, `adjacent`, `parent`, `concat`, `VIIDom`.
  Longer than every neighbouring token, and it needs capitals to stay
  clear of the lowercase-cluster reading, since every letter in
  "accessor" is a legal one.

Whichever is chosen, CST is attested community usage and the six
accessors are not, so a rule that keeps CST and treats the accessors
differently is defensible but leaves one named head among seven.

**Options, on the coinages already in use.** ANT and ULT against the
community's FRM and UNF, BEN1 against `1:BEN`.

- Keep them and record the divergence in `ERRATA.md`, so a reader can
  tell a departure from a transcription.
- Adopt the community forms, on the ground that the namespace has one
  reader and they are already writing in it.
- Leave them unmarked, which is the present state and the reason this
  entry exists.

**Options, on CNT.** G41's reading applies unchanged: an affix carries
a degree and an Aspect does not, so `CNT/3` and a bare `CNT` are
distinguishable wherever both could stand.

- Live with it on that reading, as G41 does for VRF.
- Rename on our side, which departs from both sources at once.
- Raise it upstream, where the two names were assigned.

**Options, on catching the next one.** Nothing currently notices when
`tools/sync_lexicon.py` pulls an affix abbreviation onto a grammar
code or onto one of our coinages.

- A test over `data.json` that fails on any such overlap.
- A warning from `sync_lexicon.py` at sync time.
- Nothing, and find out from a gloss.

### G52. No affix's gradient type is stated anywhere

§3.5.0.1 opens: "Each of the V_XC_S affixes falls into one of seven
gradient types". The type is not decoration. It says how the nine
degrees relate to each other, which is what a reader needs in order to
use a degree they have not memorized: Type A1 runs from one extreme to
the other, Type C oscillates and turns over at degree 5, Type 0 has no
order at all and its nine degrees are just nine labels.

Three things are supposed to convey it, and none does.

**The document does not print it.** Across 141 pages and 528 affixes,
the strings "Type 0", "A1", "A2", "B", "C", "D1" and "D2" occur only in
the changelog and in the four paragraphs of the "Seven Gradient Types"
section itself. Not one affix heading carries a type.

**The colour coding is absent.** §3.5.0.1 says the types "have been
color-coded for ease of recognition", which would carry the information
that the text does not. The published PDF has three text colours — blue,
black and green — for seven types, and they do not track type. Taking
the 303 affix headings whose colour can be read off the page against
the types the community sheet assigns:

| | 0 | A1 | A2 | B | C | D1 | D2 |
|---|---|---|---|---|---|---|---|
| black | 46 | 5 | 0 | 2 | 6 | 8 | 3 |
| blue | 113 | 19 | 0 | 15 | 10 | 47 | 2 |
| green | 13 | 0 | 1 | 1 | 3 | 4 | 2 |

Every colour holds every type. Whatever the three colours mark, it is
not this.

**The phonological signature does not determine it.** Each type is
given a signature — A1 is "a bi- or tri-consonantal form beginning with
-r or -ř", D2 "ends in -x, or two or more consonants ending in -m", and
so on — with D1 as the residue, "any consonant-form that does not
phonologically conform to the other types". Stated that way the
signature is not a mnemonic, it is a definition, and it fails twice.

It is **not exhaustive of one form**: 27 of the 528 C_S forms satisfy
two signatures at once, and the source resolves exactly one such
collision, that -rh and -řh are A1 rather than A2. `çx` begins with ç
(B) and ends in x (D2); `rm` begins with r (A1) and ends in m (D2);
`řs` begins with ř (A1) and ends in a sibilant (0). The sheet's Type
cell for `çx` is the only one of the 528 left blank.

And it **disagrees with the only other determination available**. Six
of the seven types agree with their signature almost perfectly, 113 of
115 affixes. Type 0 does not, at 172 of 255. The distributions differ
in a way that decides which is more likely wrong:

| type | by signature | by the sheet |
|---|---|---|
| 0 | 198 | 255 |
| A1 | 61 | 39 |
| A2 | 12 | 10 |
| B | 29 | 26 |
| C | 28 | 26 |
| D1 | 174 | 156 |
| D2 | 26 | 14 |

Quijada writes of D1 that "this is the most common type of affix".
Under the signature it is, at 174 against Type 0's 198 — near enough
that the claim reads as loose. Under the sheet it is not: Type 0 leads
by a hundred. The 29 clearest disagreements are all r-initial forms the
sheet types 0 (`rb`, `rd`, `rf`, `rg`, `rn`, `rp`, `rt`, `rļ`, `rň`,
`rţ`, `rḑ` and more), which A1's rule claims without qualification. So
the sheet's "0" is doing double duty as "unclassified", and the count
of genuinely non-gradient affixes is unknown.

**Why it matters here.** The type is the one thing about an affix this
repository carries that it cannot check. Roots, C_S forms, degrees and
abbreviations all have a source to be held against; the type has two
that disagree, and 27 forms where the source disagrees with itself.

**Options, on what to record as an affix's type.**

- **Keep the sheet's column.** Present state. It is the only per-affix
  determination anyone has published, and it is what a community reader
  will expect to see. Carries 83 values that Quijada's own rule
  contradicts, and conflates "Type 0" with "nobody classified this".
- **Derive it from the signature.** Reproducible, checkable, and
  Quijada's own words. Requires inventing a precedence order for the 27
  collisions, which he supplies for one, and would overrule a human
  judgment in 83 places with a rule that may only ever have been a
  mnemonic.
- **Carry both, and mark where they differ.** Honest and the most work;
  makes the disagreement visible to a reader instead of hiding it
  behind whichever we picked.
- **Drop the type.** It is not consulted by the parser or the glosser.
  Loses the one thing that says how to read a degree.

**Options, on the 27 ambiguous forms specifically.** Independent of the
above, since they need an answer under any of them.

- Take the signatures in the order §3.5.0.1 lists them, which is what
  the -rh/-řh rule happens to do (A1 is listed before A2).
- Break each tie on the affix's own degrees, which is how `-çx-` was
  settled in `ERRATA.md`. Correct per affix, and 27 judgments.
- Raise it upstream as the missing precedence rule it is.

**Options, on catching it.** Nothing currently notices when a synced
type contradicts the signature.

- A test over `data.json` listing the disagreements and failing when
  the set changes, the way the corpus drift guards work.
- Nothing, and let the two sources disagree unobserved.

## Lexicon

*Empty.*
