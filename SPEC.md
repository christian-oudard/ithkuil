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
- **Unicode phonetic surface**. The natural orthography, e.g. `Malëuţřait`. Carries diacritics for stress and non-Latin consonants.
- **ASCII phonetic surface**. Digraph notation, e.g. `aa` for `ä`, `t,` for `ţ`, `sq` for `š`. A pure recoding of the Unicode form, one-to-one with characters.
- **Canonical gloss**. Hyphen-separated morphological breakdown, e.g. `S2/PRC-ml-DYN/OBJ-MSS.G-ERG`. Both the human-readable rendering and a strict authoring syntax.
- **Serialized bytes**. Compact binary form used for sentence-level persistence.

## Round-trip guarantees

Every arm has a `format → Formative → format` round-trip identity, modulo canonicalization. Non-canonical inputs parse successfully but re-encode to the canonical equivalent. A given Formative has exactly one canonical form in each format. The system does not preserve incidental user choices (e.g. shortcut form vs. expanded form when both are valid). Equivalent inputs are folded to one canonical output, by design.

## Cross-format conversion

Any format-to-format trip (e.g. gloss → Unicode surface) is the composition of two single-arm trips through the in-memory center. There is no direct format-to-format converter and no need for one. N arms give N² round-trip pairs from N converter pairs.
