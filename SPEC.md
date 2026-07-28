# Ithkuil Round-Trip Model

The system supports several representations of an Ithkuil formative, with the in-memory `Formative` value at the center. Each peripheral format has a deterministic encode and decode pairing it with the center.

```
                    Unicode phonetic  ⇄  ASCII phonetic
                          ↕
                    canonical gloss  ⇄  Formative  ⇄  serialized bytes
```

The ASCII phonetic form is a notation-only codec on top of the Unicode form (no grammar knowledge involved). It's one step further out on the phonetic arm.

## Formats

- **In-memory Formative**. The canonical value type. Every conversion routes through it.
- **Unicode phonetic surface**. The natural orthography, e.g. `Maţřëullait`. Carries diacritics for stress and non-Latin consonants.
- **ASCII phonetic surface**. Digraph notation, e.g. `aa` for `ä`, `t,` for `ţ`, `sq` for `š`. A pure recoding of the Unicode form, one-to-one with characters.
- **Canonical gloss**. Hyphen-separated morphological breakdown, e.g. `S2/PRC-ml-DYN/OBJ-MSS.G-ERG`. Both the human-readable rendering and a strict authoring syntax. Slot order carries meaning: affixes before the Ca complex apply to the stem alone, affixes after it have scope over the Ca. A Ca holding nothing but defaults is normally suppressed, but is written `{Ca}` when it must stay visible as that boundary. The gloss is entirely ASCII, including the root, which is written in the digraph notation: it has to be typable on an ordinary keyboard, since it is an authoring syntax and not only an output format.
- **Serialized bytes**. Binary form for storing text as parsed structure rather than as pronunciation. It writes what a word means, not how it sounds, so nothing in it depends on orthography or phonotactics. It carries no lexicon version and no lexicon indices, so a stored file survives lexicon updates. Slots at their grammatical default cost nothing, and a field narrower than a byte is spent in bits where that saves a whole byte, which together make it about 40% smaller than the romanized text. Raw size is what it optimises; compressing the result is a separate concern, and the layout is not shaped to suit a compressor. Grammatical states the language cannot express are not encodable: a concatenation chain costs no framing because a dependent's own concatenation marker delimits it, which in turn means a lone formative may not carry one.

## Round-trip guarantees

Every arm has a `format → Formative → format` round-trip identity, modulo canonicalization. Non-canonical inputs parse successfully but re-encode to the canonical equivalent. A given Formative has exactly one canonical form in each format. The system does not preserve incidental user choices (e.g. shortcut form vs. expanded form when both are valid). Equivalent inputs are folded to one canonical output, by design.

## Canonical surface

A Formative has exactly one canonical surface. The grammar permits several equivalent spellings of the same word, and three of them are optional shortenings the speaker may or may not take: the §3.2 Cc shortcut, the §3.8.1.2 Cn→Ca shortcut, and the §3.9.1 moved Vc glottal. The system spells the word every legal way and keeps the best one, ranked by:

1. Fewest syllables. §3.2 justifies its shortcut as "shortening the formative by one syllable", so that is the language's own measure.
2. Fewest glottal stops. A shortcut that saves no syllable but forces a glottal, as §3.6.2 does to mark the end of Slot V once the Ca is gone, is a loss.
3. Fewest characters.

Past that the candidates are indistinguishable on any measure the spec offers, so the remaining tie-breaks exist only to keep the choice deterministic: leave the word in its plain spelling, since a compression that gained nothing is work for reader and writer with no return; then the alphabetically earlier form.

Default slot values are elided regardless, and parsing accepts the non-canonical spellings too; they simply re-render as the canonical one.

The canonical surface is also composed (Unicode NFC) and lowercase. Decomposed
input and capitals parse — a capital is a sentence-position artifact and carries
no meaning in Ithkuil — and re-render composed and lowercase. The one exception
is a foreign word following a carrier adjunct: that is passthrough text, not
Ithkuil, and keeps whatever the writer typed.

## Cross-format conversion

Any format-to-format trip (e.g. gloss → Unicode surface) is the composition of two single-arm trips through the in-memory center. There is no direct format-to-format converter and no need for one. N arms give N² round-trip pairs from N converter pairs.
