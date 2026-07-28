# Changelog

Behaviour changes worth knowing about, newest first. Defects in the
published sources go in `language_reference/issues.md`; defects in our
own handling live as skipped tests beside the code they concern. This
file is for changes that alter what the parser accepts, rejects or
reads a word as.

## 2026-07-28

### Read the NAV case-accessor alternate as i'ä, not i'ë

Cases 53-60 are §3.9.1's series-3 vowel forms minus vowel-tier 8, each
with a glottal-stop. Form 9 is `ua / iä`, so NAV takes `u'a` with the
alternate `i'ä` — the form used after a w- glide. Tier 8's alternate
`ië` drops out of the range entirely.

`parse` had `i'ë` for NAV, so `i'ä` was rejected and `i'ë` accepted as
a case that cannot be written that way. The test asserting it cited no
source; it is replaced by one covering all fifteen series-3 alternates
and the negative case.

### Read a corpus word against the grammar of its own date

Eight archive words parse into an affix whose C_S holds a glottal stop,
which §3.5 forbids outright. All eight are from 2020, and every one is
explained by the archived morphology of its own date, at
`ithkuil.place/4/archive/morphology/`. Local copies of the five
relevant versions are under `$XDG_DATA_HOME/ithkuil/reference/old/`.

Three v1.3.1 rules did not exist yet.

**C_N needed a preceding glottal stop.** v0.17.0's own changelog says
where it goes: "the Pattern 1 and Pattern 2 C_N forms have been changed
in order to eliminate the need for a glottal-stop prior to C_N." Every
`'h` and `'l` in these words is a Slot VIII C_N carrying the glottal
its date required. Dropping it is the whole update.

**Bias was a suffix, not an adjunct.** v0.17.2 §3.10 gives Slot X to
"(')C_B or (')C_Y", with the C_B suffix "immediately preceded by a
glottal stop". §3.10.1 lists the forms, and they are the ones our own
§4.7 table still gives: `kšš` is CTP "What nonsense!", `gzz` is EUH
"What bliss!", `žžg` is DEJ "[dejected sigh]". §3.10.2's C_Y is an
alternate Mood/Case-Scope, where `rš` is SPC. v1.3.1 moved Bias out of
the formative into the §4.7 adjunct and dropped C_Y in favour of the
Slot VIII C_N. Since the forms did not change, only their attachment,
the update is to split the suffix off into its own word:

    Ňvailoţmá'gzz!   (2020)  ->  ňvailoţmá gzz   (v1.3.1)
                                 'merriment' + EUH "What bliss!"

**Gemination moved onto the C_A.** §3.9.1 in the 2020 documents merges
the Slot VIII end-of-affixes glottal into the following C_A, which is
where forms like `'kš` -> `kšš` come from. §3.6.1 does that job today
by geminating the C_A outright, and closes with a parenthetical that
reads as a changelog entry of its own: "Consequently, no Slot V/VII
C_S affix increment contains a geminate **any longer**."

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
corpus, no word parses into an affix whose C_S holds a glottal. The
eight 2020 words it does reject are in `corpus/discord_examples.txt`
with their sourced readings, and the ones whose update is exact are
there too as their modernised v1.3.1 forms.

### A specialized C_S-root can have a Slot V

§4.2 says a C_S-root "operates like a standard formative except that
Slots II and IV take specialized V_V and V_R forms and the Slot III
C_R form is replaced by the C_S-form of a V_X C_S affix". Slot V is not
among the exceptions, so the §3.6.1 geminated C_A means there what it
means anywhere else. We were not looking for one on this path, and read
the first cluster after V_R as the C_A, which pushed the Slot V affixes
into Slot VII and left the geminate in an affix C_S:

    ëicalçeajja
      was   Ca=lç, Slot VII = (ea, jj)
      now   Slot V = (lç, ea), Ca=j geminated to jj

Found by measuring the unenforced half of §3.5, which bars a geminated
C_S. That count drops from nine audit-corpus words to five.

**Still unenforced.** §3.5's C_S sentence has three parts left, and the
words that break each are narrower than they first looked:

- *geminated C_S*, 5 words. Two are `ggz`, which is not a §3.6.1
  geminate of anything — rule 4 gives `gz` → `gzz` — and one is
  `ltsst`. Two are §4.6.5 referential affixes, whose consonant
  inventory includes `ll` and `mm`, so any check has to apply per
  affix kind rather than blanket.
- *barred forms* `w y ç ļ ļw ļy`, 15 words. Fourteen are one author's
  paradigm series posted on one day, all with C_S `y`.
- *h-initial C_S*, 2 words.

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
