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
- **Canonical gloss**. Hyphen-separated morphological breakdown, e.g. `S2/PRC-ml-DYN/OBJ-MSS.G-ERG`. Both the human-readable rendering and a strict authoring syntax. Slot order carries meaning: affixes before the Ca complex apply to the stem alone, affixes after it have scope over the Ca. A Ca holding nothing but defaults is normally suppressed, but is written `{Ca}` when it must stay visible as that boundary.
- **Serialized bytes**. Compact binary form used for sentence-level persistence.

## Round-trip guarantees

Every arm has a `format → Formative → format` round-trip identity, modulo canonicalization. Non-canonical inputs parse successfully but re-encode to the canonical equivalent. A given Formative has exactly one canonical form in each format. The system does not preserve incidental user choices (e.g. shortcut form vs. expanded form when both are valid). Equivalent inputs are folded to one canonical output, by design.

## Canonical surface

A Formative has exactly one canonical surface. The grammar permits several equivalent spellings of the same word; the system chooses among them deterministically, with no options to flip:

- The §3.8.1.2 Cn→Ca shortcut is used whenever its conditions hold.
- Otherwise the §3.2 Cc shortcut is used whenever the slot grammar allows it. It is unavailable to framed-verbal formatives, whose antepenultimate stress needs a third syllable that the shortened form does not have.
- The §3.9.1 moved-glottal spelling is used for cases 37-52.
- Default slot values are elided.

Parsing accepts the non-canonical spellings too; they simply re-render as the canonical one.

## Cross-format conversion

Any format-to-format trip (e.g. gloss → Unicode surface) is the composition of two single-arm trips through the in-memory center. There is no direct format-to-format converter and no need for one. N arms give N² round-trip pairs from N converter pairs.
