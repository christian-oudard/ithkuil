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

Fourteen entries have been settled, and twelve of those were defects in
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

A rendered page beats the text extraction whenever a single letter is
the point. `pdftotext` reported the SAT bias as ļţ where our markdown
had lţ, and reported §1.3's ň alternates as `n͕ or ṇ`; both needed
`pdftoppm` at 600 dpi to settle, and in both cases a low-resolution
render was also wrong, losing a cedilla to antialiasing.

G2 has left this list: §3.6's bn-substitution family is decided and
implemented, and the writeup is `ERRATA.md` §3.6.

G5 and G6 have gone without a writeup, being our confusion rather
than Quijada's defect. Both read §3.6.1's nine gemination rules as a
dispatch table every C_A must match, and counted the forms none of
them names as unreachable. The rules are a default plus exceptions:
geminating a cluster means doubling its initial consonant, and the
nine say where that does not hold. Every C_A value has a geminate,
all legal and all distinct, which `allomorph/geminate_test.go` now
checks and explains so the reading is not lost a third time.

**Verified against the source:** all of `morphology.md` and
`phonotactics.md`, the Affix table and Lexicon sections, and G33, G35,
G36, G39, G40, G41. **Not yet:** the Corpus section, and those entries
below that rest on the affix and lexicon documents rather than the
grammar.

## Affix table

### A8. PIC names two affixes, and only one of them reached the spreadsheet

The chemistry section of the affix document gives four Polyatomic Ion
affixes on one line — `-cţ` PIA, `-ẓţ` PIB, `-čţ` PIC, `-jţ` PID — and
then, fourteen lines later, a fifth: **`-žţ` PIC Additional Polyatomic
Ionic Configurations**, whose nine degrees are the oxyanion series
(-ate, -ite, hypo-...-ite, per-...-ate, -ide, bi-...-ate, dihydrogen
...-ate, di-...-ate, di...-ide).

So PIC abbreviates two different affixes with different C_S forms and
unrelated meanings. The community spreadsheet carries only `-čţ`, and
`data.json` and `affixes_reference.md` follow it, so `-žţ` is absent
from our data entirely — the cluster appears there only as a root.

Not fixed in place: the affix rows come from `tools/sync_lexicon.py`
mirroring the spreadsheet, and a hand-added row would be overwritten on
the next sync. The gap belongs upstream, and the duplicate abbreviation
belongs to Quijada either way.

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

Whether one name for two forms is an error or a compression cannot be
decided from the spreadsheet. §3.9.2 splits each of its affix families
across a `w` increment and a `y` increment by case range, and these two
carry that same pair of glides, so a range split is the obvious guess.
Not yet checked against the affix document.

## Grammar

### G40. HORTATIVE and POTENTIATIVE are both named for categories they are not

§3.9.3.1 defines the two illocutions as a matched pair, separated by
whether the wish can come true:

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
is the optative, the mood of wishing, and the split between them is the
one Allen & Greenough §441 draws inside the optative subjunctive by
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

Nothing here is ambiguous and nothing in this code is wrong: HOR is
counterfactual, and exhortation to act is DIR, whose own definition
covers "an imperative command to another party to do/be something".
The entry exists because the label misleads exactly the readers who
know the term, who reach for HOR to say "let's" and get "if only".
Anything that teaches the illocutions should gloss these two rather
than list their names.

### G41. VRF abbreviates both an illocution and an affix

The morphology document cannot make up its mind. §3.9.3.1's table and
the illocution/validation matrix both say VRF; §3.9.3.2's V_K list says
**VER**; and the v1.3.2 version history says "The 3-letter abbreviation
for VERIFICATIVE Illocution has beeb changed to VER." So the rename was
made, announced, and then applied to one of the three places.

Our transcription had VRF in all three, having missed the one VER. That
is corrected: `morphology.md` now reads VER where the source does.

The collision is what the rename is presumably for. The affix document
gives the same three letters to -**ňç** "Verifiability of Info &
Trustworthiness of its Source", and reprints the illocution matrix with
VRF in it, so both senses appear on one page.

A gloss is a sequence of abbreviations with no per-token type, so the
two cannot be told apart in one: `VRF/3` is the affix at degree 3 and a
bare `VRF` is the illocution, but only because affixes are the ones
that carry a degree. The illocution list is otherwise collision-free.

This code writes the illocution **VER**, which is what the version
history instructs and what one of the three tables already does. It is
not a departure from the source, as this entry previously recorded, but
a choice between two things the source says.

### G42. §2.3 ¶5 does not say whether a chain link may take a parsing adjunct

§2.3 makes pitch accent "the means by which word boundaries may be
parsed", and ¶5 supplies a fallback for when that channel is
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
has as many stresses as it has links. `hakšiţé-alcialu'a` bears
ultimate stress on the first link and penultimate on the second.

So an adjunct cannot declare a chain's stress, and the section gives no
way out. "Any word to be parsed" would have to mean a chain link
rather than the whole chain, which would put an adjunct inside the
hyphenated word — `'e' hakšiţe-'o' alcialu'a` — and nothing sanctions
that. The alternative readings are no better: that chains simply cannot
be sung, or that a chain takes one adjunct naming only the parent's
stress and the listener recovers the rest from the Cc markers.

We refuse rather than choose: rendering a chain without stress
diacritics returns an error naming this. Everything else round-trips,
554 of the corpus words among them. Where the source leaves a construct
undetermined we would rather say so at the point of failure than pick a
spelling and have it read later as Quijada's.

### G43. §4.6.1 assigns Rule 3 to a slot its own example spells by Rule 1

§4.6.1 lists eleven examples and adds a parenthetical: "Note that the
last three of these examples illustrate that Sec. 1.7, Rule 3, applies
to Slot 2 V_C1 and Slot 3 V_C2 for Cases 37 through 52." The three are
*sme'e*, *ka'u* and *fo'we'is*.

The first two bear it out. Both have no Slot 3, so V_C1 ends the word:
**sme'e** reduplicates *e* around the glottal and **ka'u** takes it
intervocalically in the diphthong *au*, which is Rule 3 in each of its
two shapes.

The third does not. **fo'we'is** writes V_C1 as **o'**, the glottal
after the whole vowel-form, which is Rule 1. Rule 3 would give
**fo'owe'is**. Its V_C2 *is* Rule 3, as **e'i** for *ei*.

§1.7 accounts for the word as printed: Rule 1 is the default and Rule 3
overrides it only where Rule 1 would be impermissible or would leave
the glottal word-final. V_C1 has the Slot 3 **w** behind it and so is
neither. Read that way the parenthetical is loose rather than wrong,
naming §1.7 as the reason glottal stops appear in those slots at all,
not fixing which of its rules applies. The alternative is to take the
sentence at its word and call the printed example a slip.

This code follows §1.7 and the printed example: a V_C1 with a Slot 3
behind it is written by Rule 1, and a word-final one by Rule 3.

### G44. Two §2 rules moved from the prose into the tables, and we called them invented

*A correction to this list, not a defect in the source.*

`phonotactics.md` carried a rule barring **ḑs**, **ḑš**, **ḑz**, **ḑž**
and **nň**, numbered 2.23. G37 could not find it in phonotaxis v0.5.4
and concluded it had been invented in transcription. Only the newest
version's prose was read. Both halves are Quijada's.

They were prose rules in the two editions before v0.5.4:

> **v0.3 / v0.4 §2.6.** The voiced interdental fricative -ḑ- cannot be
> followed by any of the four sibilant fricatives.
>
> **v0.3 / v0.4 §2.15.** The nasal n- cannot be followed by the labial
> stops -p and -b, as they are too likely to assimilate to -mp- and
> -mb-. Nor can -n- be followed by -ň-.

Between v0.4 (June 2019) and v0.5.0 (January 2021) §2 was renumbered
from twenty-three rules to twenty-two. The ḑ rule went; the n rule
survives as v0.5.4's 2.14 word for word, less its final sentence.

The constraints did not go with them. §8's matrix of permissible
bi-consonantal conjuncts, in v0.5.4, marks every one of the five as
impermissible: the **ḑ** row's blue squares are ţ, s, š, z and ž, and
the **n** row's are p, b, c, č, ẓ, j and ň. The row totals in the
document confirm the count without reading the colour, 25 of 30 for
**ḑ** and 23 of 30 for **n**. §3.6's substitution table says the same
from the other side, carrying **ngn → ňn** as a named exception to
**[C]gn → [C]ň** whose only effect is to keep a derivation off **nň**.

So the rules were not dropped, they were moved from prose into tables,
and what we enforce is what the current document says. The lexicon
agrees: across 5,946 roots and 528 affixes, **ḑ** heads 152 clusters
over at least eight following consonants and never a sibilant, and
**n** heads 748 and freely takes s, z, š and ž but never **ň**.

What was ours is the number. v0.3 and v0.4 both end at 2.23, and their
2.23 is the w/y rule that survives as today's 2.22, so the transcription
merged two rules under a number belonging to a third.

**2.24 is not the same case.** Neither v0.3 nor v0.4 prohibits **çç**
or **ļļ**, and **ļļ** appears in their tables as a permitted form. That
one was invented, so G1's withdrawal stands. Identical provenance,
opposite answer, which is why they should not have been decided
together.

### G45. The adjunct inventories are not licensed by the phonotactics

Two of Quijada's documents disagree about what may begin a word. The
phonotactics document rules on it; the grammar document publishes
tables of adjuncts that break those rules, and prints worked examples
of them.

§4.5's four suppletive adjuncts are **hl**-, **hm**-, **hn**- and
**hň**-, described there as "the initial **h**+consonant with no
subsequent consonant forms", with §4.5.4's own examples reading
*hňa, hňei, hňo, hňe'e, hňa'u, hňi'a*. §3.8.1.2 adds **hňw** to the
C_N forms that can move into Slot VI.

Phonotactics §3.2.7 permits word-initial **h**- to be followed by "-l
or -r, the nasals -m or -n, or by -w". Not -**ň**. §3.3.5 limits
word-initial triconsonantal **h**- conjuncts to hlw, hrw, hmw, hnw,
hmy, hny and four geminates, so **hňw** is unlicensed too. The
sequence **hň** does not occur anywhere in the phonotactics document,
in any position.

The §4.4 registers are the same story from the vowel side: their
adjuncts are a single vowel-form after **h**-, and every one of them
is fine, but only because the table stops where it does.

The bias adjuncts of §4.1 are the largest case. A bias adjunct is a
bare consonant conjunct standing alone as a word, a shape §3 never
contemplates, every rule there being written about a conjunct with a
vowel-form beside it. Taken at its word, 34 of the 61 published forms
are unlicensed word-initially: §3.2.9 grants **l**- and **r**- only
-w or -y, which fails ACC *lf*, ANP *lst* and nine more; §3.2.8 grants
nasals a liquid or approximant, which fails ATE *ňj*, RSG *msf* and
nine more; **ř**- is granted no word-initial pair at all, failing APB
*řs*, DOL *řřx* and IVD *řřn*; §3.2.1, §3.2.2, §3.3 and §3.3.4 take
one apiece. None is hypothetical: every one is attested standing alone
in the community corpus, *pļļ* 291 times, *msf* 127, *kçç* 48.

Read together the documents are consistent if §3's word-initial rules
are scoped to words that have a vowel in them, and if the adjunct
tables are authoritative for the shapes they publish. That is the
reading this code takes: `phonology.ParseWord` exempts a lone consonant
conjunct from the cluster rules, and the sweep in
`roman/inventory_test.go` holds the adjunct classes to §1's vowel rules
but not §2's and §3's cluster rules. It is a reading, not something the
source states, and the alternative is that §3.2.7 simply omits -ň.
### G47. §2.10 and the §8 grid disagree about ç before a voiced affricate

§2.10 states the rule in prose, with a reason:

> The voiceless palatal fricative -**ç**- cannot be preceded or followed
> by a a sibilant fricative (**s**, **z**, **š**, **ž**), nor preceded
> by a sibilant affricate (**c**, **ẓ**, **č**, **j**), nor followed by
> a voiced sibilant affricate (**ẓ**, **j**).

§8's grid, which tabulates the bi-consonantal conjuncts that can be a
C_R root or a C_S affix, marks **çẓ** and **çj** permissible. Those are
exactly the two forms the last clause bars.

The grid is not casual about ç elsewhere in the same row: `çs`, `çš`,
`çz`, `çž`, `çļ` and `çh` are all marked impermissible, matching the
other clauses of §2.10 exactly. Only the voiced-affricate clause is
contradicted, and it is the one clause with a stated phonetic reason.

This is the whole of the disagreement between §8 and §§1-7. Generating
the grid from the other rules reproduces it cell for cell in 26 of the
27 rows — 810 checks — and the ç row is the single exception.
`phonology.TestSection8GridMatchesRules` pins that, and follows the
prose, so `çẓ` and `çj` are rejected.

### G48. None of the four conjunct tables agrees with its own arithmetic

§§8-11 tabulate the permissible consonant conjuncts. Each is derived
from the rules in §§1-7, and each carries per-row totals and a grand
total. In all four the rows, the row totals and the grand total are
three different answers.

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
  is its third-consonant count alone; §10's `rř` + 21-consonant +
  **n** + `wy` row prints 42, which is 21 x 2. Both omit the leading
  `r ř` from the product, and in both cases the neighbouring rows with
  a single-consonant first column are correct.
- **A digit typo.** §9's `c` + **c** row prints **223** where its
  contents give 23. On its own that accounts for 200 of §9's 162-form
  gap; correct it and the printed totals sum to 4983.
- **Parallel blocks that disagree.** §11 tabulates **z** and **ž** in
  blocks with identical set structure. The `mn`/`wy` row prints 45 in
  the z block and 36 in the ž block; 36 is the product.
- The other eight are off by amounts with no evident cause, the largest
  being §10's `rřl` + `bgv` + **z** row, printed 64 where its sets give
  144.

Nothing here changes what is permissible: the rows say which conjuncts
exist, and the totals are commentary on them. It does mean no count
Quijada prints for these tables can be quoted as authoritative, and
that the widely-cited "679 bi-consonantal conjuncts" is one of the
five-row disagreements rather than a figure the grid supports.

### G49. §4.5.3's naming-adjunct examples hold a form from the adjunct above

Each of §4.5's four suppletive adjuncts prints six example words built
on its own C_P: `hla, hlei, hloa, hle'e, hla'u, hli'a` for CAR on
**hl**, and the same six vowel-forms on **hm** for QUO, **hn** for NAM,
**hň** for PHR. Two entries break the pattern:

- NAM's second example is **hmei**, which is QUO's. On the pattern it
  should be `hnei`, and every other NAM example is `hn`-initial.
- PHR's third example is **hňo** where the other three rows have the
  Case-3 form `-oa`. It should be `hňoa`.

Both are single-word slips in an otherwise mechanical list, so nothing
rests on them; the C_P values themselves are stated separately in the
same table and are not in doubt. Recorded because the transcription
carries them as printed rather than repairing them, and a reader
building a test set from these examples would otherwise get one
adjunct's form filed under another's.

### G50. The phonotactics document's vowel chart has a tenth vowel

§1 of the phonotactics document prints a vowel chart with **ï** in the
high central unrounded cell, beside i, ü and u. The grammar document's
§1.1 chart has the same three and no fourth; the vowel-form table of
§1.6 is built from nine vowels, not ten; and ï appears nowhere else in
the phonotactics document, in any rule, in any conjunct table, or in
any root or affix in the lexicon.

So nothing depends on it and nothing else corroborates it. It is
transcribed as printed, in the cell it occupies, because the cell is
the whole of the claim: an implementation that added a tenth vowel on
this evidence would have no vowel-form to put it in.

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
and UNF. How the §3.9.2 affixes should be glossed is open: an invented
code adds a fourth party to a namespace already unmanaged, the C_S
cluster puts phonetics in a gloss that is phonetics-free everywhere
but the root, and spelling the source's own words out is legible but
abandons the shape every other token has. A guard that failed when a
synced affix abbreviation landed on a grammar code would at least
surface the next CNT at sync time rather than in a gloss.

## Lexicon

### L1. 453 spreadsheet roots are shapes §§9-11 do not admit

§§8-11 enumerate the consonant conjuncts that can be a root or an
affix. Putting the 5,895 roots of the community spreadsheet through
them, 453 are shapes no row admits.

| root length | outside §§9-11 | of |
|---|---|---|
| 1-2 | 0 | 670 |
| 3 | 128 | 2093 |
| 4 | 236 | 2456 |
| 5 | 89 | 676 |

The split at two consonants is the first thing worth noticing. §8 is a
grid derived from the pair rules of §§2-7, so a bi-consonantal root
that our validator accepts is one §8 accepts, and every short root
passes. §§9-11 are independent tables that no rule generates, and the
whole of the disagreement is there.

The departures are systematic, not scattered. `-pf-` accounts for
fourteen of the tri-consonantal cases: §9 gives medial **p** a third
consonant **f** only after the initials `sšç`, and the spreadsheet also
writes `ţpf`, `žpf`, `ḑpf`, `ẓpf`, `čpf`, `jpf`, `kpf`, `gpf`, `dpf`,
`xpf`, `zpf`, `ňpf`, `ļpf` and `cpf`, plus `pfc` and `pfč` with the
pair leading. `Cml`/`Cmr`/`Cmř` is a second family of the same kind:
`cml`, `sml`, `vml`, `zml`, `ẓml` and their `r` and `ř` partners. Two
affixes depart as well, `xḑr` and `čḑr`, sharing a final `-ḑr` that §9
gives no initial at all.

A family that large is not a run of typos. Either the spreadsheet
extended a series past what §9 sanctions — which is how the biological
sections are built, per the Lexicon entries in `ERRATA.md`, one taxon
per suffix in a fixed series — or §9's rows are incomplete in a
patterned way. G48 is a reason to hold the tables lightly: none of the
four agrees with its own arithmetic.

Deciding this is upstream's call. What is not in question is that the
check should exist: `phonology.RootConjunctLegal` implements §§8-11 and
`fbm` is the case that motivated it, a conjunct both of whose pairs §8
permits, that appears in no §9 row, and that a speaker asked to say
reported impossible. It is not yet enforced anywhere, because enforcing
it as things stand would refuse 8% of the vocabulary.

`lexicon/root_shape_test.go` pins the counts, so a re-sync that changes
them says so.
