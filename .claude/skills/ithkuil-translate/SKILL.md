---
name: ithkuil-translate
description: Translate Ithkuil into natural English, or read a gloss back into English. Use when asked what an Ithkuil word or sentence means, or when handed a morphological gloss to render.
---

Translate by parsing first. Never translate from the romanization by eye:
Ithkuil packs a clause into one word, and the categories that decide the
English are unwritten defaults as often as they are visible letters.

## 1. Parse

```bash
ithkuil parse "<the Ithkuil text>"
```

That prints, per word, the gloss, the slot-by-slot phonetic breakdown, the
root's lexical meaning, and a CATEGORY/CODE/NAME/MEANING table. The table is
the raw material; everything below is about turning it into a sentence.

If a word fails to parse, say so and stop. A word the parser rejects is not
a word you can translate, and guessing from the letters produces confident
nonsense.

Try restoring an elided default before you give up on it, though. Slot II is
written `(a)` for Stem 1 PRC — the parenthesis means optional — so a word
whose author dropped it can begin with a cluster §3.2 forbids and still be one
vowel away from legal. `dnivó` is rejected word-initially; `adnivó` parses as
`dn-STA.OBJ.EXS-N-ASR.CVN-ULT`. Restoring a default is reading; guessing at
the letters is not. Say which you did.

## 2. Look up what the codes do to English

For every CODE in the table:

```bash
ithkuil search --exact "<CODE>"
```

An entry's `explanation` is the fuller reading of the value and its
`guidance` says how it lands in English, which is not what the grammar
reference says: the grammar defines the category, the guidance corrects the
misreading. 160 of the 294 values carry one. The rest are absent because
they hold no surprise, so absence is not a gap to fill by invention.

`docs/reference/morphology.md` is the grammar itself, for when a value has
no guidance and the category is doing real work in the sentence.

## 3. Read the gloss structure

Separators are not decoration and they are not interchangeable:

- A space or a separate parse block starts a new word. Affixes bind inside
  their own word only.
- `-` separates morphological slots within a word.
- `/` stacks roles inside one slot, as in `1M/BEN`.
- `+` conjoins sub-pieces sharing a slot, as in `IND+TSP`. `+` is not `-`.
- Quoted text is the root's lexical meaning, not a word of the answer.
  Translate it in context.

## 4. The categories that decide the sentence

Four slots settle the shape of the English before any word of it is chosen,
and each is easy to read past because the gloss states it without emphasis.

- **Stress is a slot.** Penultimate is an unframed nominal, ultimate an
  unframed verbal, and antepenultimate is a FRAMED relation — a subordinate
  clause. `ufçala` and `úfçala` hold the same slots; only the second glosses
  `-ANT`. When a constituent seems to hang loose and you reach for an
  em-dash or a "which is", read the stress: if it is antepenultimate the
  language has already said "frame", and the obliques after it belong inside
  that frame rather than beside it.
- **Validation is content.** A verb's Vk is not bookkeeping. A passage whose
  verbs all carry CVN is marked as told-lore — the register English opens
  with "once upon a time" — and dropping it loses the frame the whole text
  is spoken in.
- **Ca decides number and genericity.** MSC on a root meaning one instance
  is what yields a mass noun: "one human head hair" in a connected multiplex
  is *hair*. Nomic perspective is what makes a predication a standing
  property rather than an event. Choosing "a hair" or "she walks" from the
  root alone skips the slot that decides it.
- **Context licenses the non-literal root.** A root that reads absurdly
  specific in EXS is often doing representational work: RPS turns "social
  introduction" into "presents". Check Vr before judging a root too narrow
  for the sentence it is in.

## 5. Write English, not a calque

Prioritize standard English syntax and concision. Do not hold a
slot-by-slot or affix-by-affix mapping when it yields stacked prepositional
phrases: synthesize into compound nouns, noun adjuncts, or possessives when
that is what an English speaker would say.

Do not add meaning the gloss does not license. No extra participants, no
tense the categories do not carry, no causation, no epistemic framing. The
commonest failure is a matrix clause bolted onto a clause that already
carries the sense: OBS does not mean "I perceive that", and a declarative
illocution does not mean "I hereby declare that".

Do not drop meaning either. A category the English cannot carry naturally is
worth a note under the translation rather than silent deletion.

Make the preposition carry the case. An oblique case asserts a specific
relation and English "of" asserts none, so flattening one into a genitive
quietly drops it — and PUR is directional, so "a description *of* a language"
and "a design *for* a language" disagree about which noun is the artifact and
which the goal. Naming the case correctly in the notes and then writing "of"
anyway is the easy version of this mistake.

Invent no participant the cases do not give you. A patient in ABS with no
agent anywhere is agentless: English passive is the honest rendering, and
supplying a "he" to head the sentence adds a party the text does not have.
The same restraint governs determiners — a DCD on one word is not a "this" on
its neighbour.

Ithkuil marks no tense. Version is process against completed whole, not
present against past, so nothing licenses an English past unless an aspect or
a temporal affix puts it there. Choose a tense for the discourse, then hold
it: a passage that opens in the present and continues in the past reads as a
misreading even when the propositions are right.

## 6. Answer

Give the English sentence first, plainly. Then, when the reader would want
it, a short justification naming the categories that decided the wording,
particularly any that are high-salience: validation and modality, essence,
case roles, affix degree. State ambiguity where the gloss is genuinely
ambiguous rather than picking silently.

For a multi-word text, translate each word, then the whole. The full
sentence is the deliverable; the per-word readings are the evidence.

## 7. Checking a translation against a reference

Composing has a mechanical check — parse the word back. Translating has
none, so the only honest one is to commit before you look. Once a proposed
meaning is in front of you it cannot be unseen: the reading you produce
afterwards is recognition wearing the clothes of analysis, and it will feel
exactly as earned as the real thing.

So write the translation, and the reasoning that got you there, before the
reference enters the context. This is a live hazard rather than an exercise
rule — a supplied text often arrives with a translation beside it, a
neighbouring column, a "this means roughly", a familiar passage — and the
discipline is to parse first and read the offered meaning afterwards, as
evidence to weigh rather than an answer to arrive at. Where the material
cannot be separated by hand, a subagent can hold the reference and score the
committed answer, which keeps it out of the context entirely.

Weigh what comes back. A published translation is often the English a
community translator worked *from*, not a gloss of what they actually wrote,
so a mismatch may be their Ithkuil failing to encode their source rather than
your reading failing to decode it. Say which you think it is, and where the
morphology is clear, side with the morphology: HUM/4 is "as planned by
humans", so "human-designed" beats a reference reading "human-usable".

## Going the other way

To render English into Ithkuil, or a gloss back into a word:

```bash
ithkuil compose -- "<gloss expression>"   # -- first: a gloss starts with -
ithkuil define "<English word>"     # English -> candidate lexical cores
ithkuil search "<term>"             # grammar inventory, roots, affixes
```

Compose accepts the same canonical gloss syntax the parser emits, so a gloss
round-trips. Vary stem and version rather than defaulting everything, or
every word you build starts with `a-`.

Compose by round-tripping, the way translation parses first. `compose` emits
a well-formed word even when the mood or case you asked for was dropped or
reread as something else, so a word that builds is not a word that means what
you meant. Parse every word you compose and confirm the gloss carries what you
intended. Three traps account for most of the drift:

- **Verbal or nominal decides whether a mood survives.** A bare formative is
  nominal, so its Cn is read as case-scope, and a mood written onto it — SUB
  for "could", and the rest — is silently reread as a case-scope value. Give
  the formative an illocution (ASR) or a validation (OBS, INF, …) to make it
  verbal, and the mood holds. `SUB-rt,t` round-trips as `rt,t-CCA`;
  `SUB-rt,t-INF` round-trips as `rt,t-SUB-ASR.INF`, mood intact.
- **Participants are case-marked, not ordered.** Formatives left in the
  default THM and set side by side are a list of disconnected concepts, not a
  clause. The roles live in the cases — ERG the agent, OGN the source, PRN
  "about", DAT the recipient, AFF the experiencer. A reciprocal's asymmetry,
  who learns from whom, exists only in that marking; drop it and both halves
  collapse to the same thing.
- **Epistemic framing is a validation, not a second verb.** "I think", "I
  hear", "I saw" fold into INF, RPR, OBS — the composing mirror of the rule
  that OBS is not "I perceive that". Do not build a "believe" verb over a
  clause a validation already carries.

Confirm a degree's sense before spending it: `search --exact` the affix and
read the degree line. `ITY/9` is "too intense", not "a lot"; the degree you
want is often one below the extreme. And `define`'s English index is partial —
when it names nothing, `search` the meaning text, and where no root fits, note
the gap under the translation rather than inventing one.
