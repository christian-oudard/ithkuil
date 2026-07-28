# Changelog

Behaviour changes worth knowing about, newest first. Defects in the
published sources go in `language_reference/issues.md`; defects in our
own handling live as skipped tests beside the code they concern. This
file is for changes that alter what the parser accepts, rejects or
reads a word as.

## 2026-07-28

### Gemination moved onto the C_A, and it dates the corpus

§3.6.1 marks the end of Slot V by geminating the Slot VI C_A complex,
and closes with a parenthetical that reads as a changelog entry:

> Consequently, no Slot V/VII C_S affix increment contains a geminate
> **any longer**.

Before v1.3.1 the affix increment carried the gemination, and a glottal
stop marked the affix boundary. Both jobs now belong to the geminated
C_A. That single change explains a whole class of archive words that
look like defects in this code and are not:

- **Eight words** parse into an affix whose C_S holds a glottal stop,
  which §3.5 forbids outright. All eight are from 2020, three years
  before v1.3.1, and six are one author working through a translation
  project. Three end in a geminated affix increment that is a real
  affix under the old reading: `kšš` is BEH, `žžg` is OLF, `gzz` is
  XOH. They are in `corpus/discord_examples.txt` marked incorrect,
  with the era and the old reading.

The practical rule: **a corpus word's date decides which grammar it is
written in.** `tools/discord_archive/words.py` filters the audit corpus
to 2023 and after for exactly this reason. Word lists hand-copied into
test files predate that filter and are not covered by it, so date a
word before treating it as evidence of a defect.

### An affix C_S may not contain a glottal stop

§3.5, enforced in `slots.affixesVxCs`. §1.7 Rule 1 puts a glottal
between a vowel and a consonant after the vowel-form, so the V_X in
front carries it, never the C_S behind. We used to build the affix
anyway; the renderer then had nowhere to put the glottal, dropped it,
and the affix came back a degree off in a different slot. Round-tripping
through an impossible affix was never evidence of a correct reading.

Costs nothing in the language we implement: across the 3657-word audit
corpus, no word parses into an affix whose C_S holds a glottal.

**Still unenforced:** the same §3.5 sentence also bars a *geminated*
C_S, and that half is open. Nine audit-corpus words parse into one, and
unlike the glottal case they are current — 2024 to 2026, seven
different authors — so this is a live question about the language we do
implement. Every geminate un-geminates to a real affix (`ggz`→XOH,
`ddy`→S18, `dd`→SCS, `jj`→X10), which is what a §3.6.1 geminated C_A
looks like: we are choosing the wrong cluster for the C_A and pushing
the Slot V affixes into Slot VII. In *ëicalçeajja* the C_A should be
`jj`, geminated from `j`, with `lç`+`ea` as a Slot V affix; we read
C_A `lç` and put `ea`+`jj` in Slot VII instead.

### A concatenated formative's Slot IX is a Format

§3.1.3. Stress on a dependent does not choose between a nominal and a
verbal reading, it chooses the case group: ultimate promotes the vowel
into cases 37-68, which a dependent writes without the glottal because
Slot I already spends one on the no-concatenation C_C. We read the
vowel as a V_K and rejected the word. PRN joins THM as an elidable
trailing `-a-`.

### A lone concatenation marker is not a word

§3.1.8 joins a chain with a hyphen, so a word with no hyphen cannot
carry a Slot I C_C. Fourteen such words now classify as UnknownWord
rather than as standalone formatives; they are foreign names in Ithkuil
letters, or a dependent someone quoted without its parent.
