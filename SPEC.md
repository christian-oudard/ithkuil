# Ithkuil Round-Trip Model

The system supports several representations of an Ithkuil word, with an in-memory grammar value at the center. Each peripheral format has a deterministic encode and decode pairing it with the center.

A formative is the richest of the word classes and `Formative` the largest of the types, but it is one of several: a referential, a bias adjunct, a register marker and the rest each have a type of their own in the grammar layer, and the same four arms. Nothing in the centre records how a word was spelled.

```
                     romanization  ⇄  ASCII digraphs
                          ↕
                  canonical gloss  ⇄  grammar value  ⇄  serialized bytes
```

The ASCII digraphs are a notation-only codec on top of the romanization, with no grammar knowledge involved. That is why they hang off it rather than off the centre: one step further out on the same arm, not an arm of their own.

## Formats

- **In-memory grammar value**. The canonical form. Every conversion routes through it. One type per word class, with `Formative` the largest; sum types cover the variation within a class, so a value that the language cannot express should not be constructible.
- **Romanization**. The Latin-alphabet form, e.g. `Maţřëullait`. Carries diacritics for stress and non-Latin consonants. It writes how a word sounds, which is what separates this arm from the gloss and the bytes. Ithkuil's own script is morpho-phonemic and writes morphemes rather than sounds, so this is a romanization of the language, not its natural orthography.
- **ASCII digraphs**. A keyboard encoding of the romanization, e.g. `aa` for `ä`, `t,` for `ţ`, `sq` for `š`, with a postfix `/` for stress: `ee/` for `ê`. One-to-one with characters. The full table is in the README.
- **Canonical gloss**. Hyphen-separated morphological breakdown, e.g. `S2.PRC-ml-DYN.OBJ-MSS.G-ERG`. Both the human-readable rendering and a strict authoring syntax. Slot order carries meaning: affixes before the Ca complex apply to the stem alone, affixes after it have scope over the Ca. A Ca holding nothing but defaults is normally suppressed, but is written `{Ca}` when it must stay visible as that boundary. The gloss is entirely ASCII, including the root, which is written in the digraph notation: it has to be typable on an ordinary keyboard, since it is an authoring syntax and not only an output format. Its punctuation follows the rule below.
- **Serialized bytes**. Binary form for storing text as parsed structure rather than as pronunciation. It writes what a word means, not how it sounds, so nothing in it depends on orthography or phonotactics. It carries no lexicon version and no lexicon indices, so a stored file survives lexicon updates. Slots at their grammatical default cost nothing, and a field narrower than a byte is spent in bits where that saves a whole byte, which together make it about 40% smaller than the romanized text. Raw size is what it optimises; compressing the result is a separate concern, and the layout is not shaped to suit a compressor. Grammatical states the language cannot express are not encodable: a concatenation chain costs no framing because a dependent's own concatenation marker delimits it, which in turn means a lone formative may not carry one.

## Gloss punctuation

Each mark in the canonical gloss has exactly one job. A token's kind therefore follows from its shape, and no lexicon lookup is needed to decide what it is.

- `-` separates slots: `S2.CPT-ml-ERG`
- `.` joins category values inside one slot: `S2.CPT`, `DYN.OBJ.FNC`, `MSS.G`, `RCP.HYP`, `ASR.RPR`
- `/` binds an argument, a degree or a case, to a head: `DEV/3`, `ACC/INS`, `(1m)/AFF`, `1m/BEN`, `[2m]/IND`
- `_` trails a modifier, currently only the affix Type: `t/1_2`, `IAC/PRP_3`, `DSV_END`
- `:` tags a structured body: `Ca:MSS.G` and `Ca:{Ca}` for a stacked Ca, `NOM:1m` for a referent category
- `()` wraps a head built from referents or from a Cs: `(1m+2p/BEN)`, `(CTR)/1`
- `+` joins referents: `1m+2p`
- `{}` marks something structural rather than a morpheme: `{Ca}`, `{parent}`
- `[]` marks a word-level head that is not a root: `[QUO]`, `[1m+2p]`

The rule is what keeps the syntax extensible. Anything that binds a case to a head reads `HEAD/CASE`, whether it comes from §3.9.2, §4.6.5, or the second referent of a §4.6.1 referential, so the three do not have to be told apart by name. A construct that would need a new sense of an existing mark needs a new mark instead.

The same rule decides when a construct gets no mark at all. A §4.6.1 second case with no referent of its own stacks onto the head rather than binding to anything, so it stays a plain slot: `1m-THM-ERG` stacks, `1m-THM-[2m]/IND` binds. And a §4.6.3 suppletive-headed referential needs no sigil to tell it from the carrier adjunct it resembles, because a carrier adjunct holds one case and nothing else — the extra slots are themselves the signal.

Grammatical values that carry no punctuation of their own follow from this: the reach and scope markers inside `{}` are single words for that reason, since a `/` or `.` there would claim a job it does not have and a `-` would split the token.

## Round-trip guarantees

Every arm has a `format → Formative → format` round-trip identity, modulo canonicalization. Non-canonical inputs parse successfully but re-encode to the canonical equivalent. A given Formative has exactly one canonical form in each format. The system does not preserve incidental user choices (e.g. shortcut form vs. expanded form when both are valid). Equivalent inputs are folded to one canonical output, by design.

## Canonical romanization

A Formative has exactly one canonical romanization. The grammar permits several equivalent spellings of the same word, and three of them are optional shortenings the speaker may or may not take: the §3.2 Cc shortcut, the §3.8.1.2 Cn→Ca shortcut, and the §3.9.1 moved Vc glottal. The system spells the word every legal way and keeps the best one, ranked by:

1. Fewest syllables. §3.2 justifies its shortcut as "shortening the formative by one syllable", so that is the language's own measure.
2. Fewest glottal stops. A shortcut that saves no syllable but forces a glottal, as §3.6.2 does to mark the end of Slot V once the Ca is gone, is a loss.
3. Fewest characters.

Past that the candidates are indistinguishable on any measure the spec offers, so the remaining tie-breaks exist only to keep the choice deterministic: leave the word in its plain spelling, since a compression that gained nothing is work for reader and writer with no return; then the alphabetically earlier form.

Default slot values are elided regardless, and parsing accepts the non-canonical spellings too; they simply re-render as the canonical one.

The canonical romanization is also composed (Unicode NFC) and lowercase. Decomposed
input and capitals parse — a capital is a sentence-position artifact and carries
no meaning in Ithkuil — and re-render composed and lowercase. The one exception
is a foreign word following a carrier adjunct: that is passthrough text, not
Ithkuil, and keeps whatever the writer typed.

## English index

The lexicon writes root meanings in English, so it can be read backwards: an English word looks up the Ithkuil lexical cores that express it. This arm is deliberately not a round trip. One English word yields zero, one, or many senses, and a sense reads back as a whole gloss phrase rather than as the word looked up.

A sense is a lexical core only: root, stem, version, function, specification, context. Case, illocution, and the rest of what a formative carries belong to the sentence, not to a dictionary entry.

Headwords are read out of the gloss text by a fixed rule. A semicolon separates distinct senses within one gloss; a slash separates interchangeable wordings of a single sense; parenthesised and bracketed material is explanation and not part of the headword; a leading "to" or "to be" is an infinitive marker; a trailing footnote mark is not part of the word. Commas do not separate senses, because in these glosses they almost always separate modifiers inside one sense. Matching is case-insensitive against the whole headword, not a substring.

Coverage of English is partial and stays that way. The index says what the lexicon already happens to name in English; it is a measurement of that, not a claim to be a dictionary of English.

## Cross-format conversion

Any format-to-format trip (e.g. gloss → romanization) is the composition of two single-arm trips through the in-memory center. There is no direct format-to-format converter and no need for one. N arms give N² round-trip pairs from N converter pairs.
