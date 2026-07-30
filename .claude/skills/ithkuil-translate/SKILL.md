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

## 2. Look up what the codes do to English

For every CODE in the table, check `docs/grammar_explanations.md`. Entries
there say how a category lands in English, which is not what the grammar
reference says: the grammar defines the category, the supplement corrects the
misreading. About 200 categories have one. The rest are absent because they
hold no surprise, so absence is not a gap to fill by invention.

`docs/reference/morphology.md` is the grammar itself, for when the supplement
is silent and the category is doing real work in the sentence.

## 3. Read the gloss structure

Separators are not decoration and they are not interchangeable:

- A space or a separate parse block starts a new word. Affixes bind inside
  their own word only.
- `-` separates morphological slots within a word.
- `/` stacks roles inside one slot, as in `1M/BEN`.
- `+` conjoins sub-pieces sharing a slot, as in `IND+TSP`. `+` is not `-`.
- Quoted text is the root's lexical meaning, not a word of the answer.
  Translate it in context.

## 4. Write English, not a calque

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

## 5. Answer

Give the English sentence first, plainly. Then, when the reader would want
it, a short justification naming the categories that decided the wording,
particularly any that are high-salience: validation and modality, essence,
case roles, affix degree. State ambiguity where the gloss is genuinely
ambiguous rather than picking silently.

For a multi-word text, translate each word, then the whole. The full
sentence is the deliverable; the per-word readings are the evidence.

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
