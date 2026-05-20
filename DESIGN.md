# Parse/render layering

The parse and render pipelines are organized as a stack of pure
transformations. Each layer round-trips: parse and render are
mirror-image functions on the same data.

```
Surface text          "öhwoňó"
       ↕ Layer A: strip/apply acute, return stress position
Bare text + stress    "öhwoňo", Ultimate
       ↕ Layer B: split/join vowel ↔ consonant runs (no grammar)
Conjuncts + stress    ["ö","hw","o","ň","o"], Ultimate
       ↕ Layer C: structural classification (formative shape /
       ↕          modular / referential / shortcut form)
Slot-labelled         [(Vn₁,"ö"),(Cn₁,"hw"),(Vn₂,"o"),(Cn₂,"ň"),(Vh,"o")]
segments + stress     + Stress=Ultimate
       ↕ Layer D: surface → grammatical values via lookup tables
Grammar object        grammar.ModularAdjunct{Pairs:[…], Final:"o"}
       ↕ Layer E: context-dependent semantics (V_N vs V_H,
       ↕          Mood vs CaseScope, etc.)
Semantic structure    {Pairs decoded, Slot 4 = V_H scope marker}
```

## Package layout

| Layer | Package                                | Status            |
|-------|----------------------------------------|-------------------|
| A     | `surface/` (stress.go)                 | ✅ extracted, round-trip tested |
| B     | `surface/` (conjunct.go)               | ✅ extracted, round-trip tested |
| C     | inside `fullparse/`, mirrored in `render/` and `inspect/` | ⚠️ not extracted; conceptually present |
| D     | mostly in `parse/` (ParseSlotII, ParseSlotIV, allomorph.ParseCa, etc.) + `render/` (SlotIIToVv, SlotIVToVr, etc.) | ⚠️ paired but not formally typed as one layer |
| E     | inside `inspect/`, `render/`, `gloss/` | ⚠️ entangled with C/D |

## What `surface/` owns

Pure byte/rune-level work. No grammatical knowledge.

- `Strip(word) → (bare, Stress)` — drop acute/circumflex, report stress.
- `Apply(bare, Stress) → word` — place acute/circumflex per stress.
- `SplitConjuncts(word) → []string` — alternating vowel/consonant runs.
- `JoinConjuncts([]string) → string` — inverse (just `strings.Join`).
- `MergeGlottalVowels([]string) → []string` — re-glue V-'-V triples
  that conjunct splitting separated.
- `IsVowel(rune)`, `IsVowelConjunct(string)`, `IsConsonantConjunct(string)` —
  rune/string classification.

Round-trip tests in `surface/stress_test.go` and
`surface/conjunct_test.go` exercise the inverses on the working
corpus (Malëuţřait, fkhalo, mzalörmëiňva, walurx, ëilal, ealali,
ihwe, öhwoňó, …).

## What `parse/` owns now

After the layering work, `parse/` is the home of grammatical
decoders for individual slot positions. Nothing text-level lives
here anymore.

- `ParseSlotII(vowel) (SlotII, bool)` — Vv → (Stem, Version)
- `ParseSlotIV(vowel) (SlotIV, bool)` — Vr → (Function, Spec, Context)
- `ParseVnAspect`, `ParseVnValence`, `ParseVnPhase`, `ParseVnEffect`,
  `ParseVnLevel` — Vn series
- `ParseCnMood`, `ParseCnMoodP2`, `ParseCnCaseScope` — Cn → mood
- `ParseCase(vowel) (Case, bool)` — Vc → Case
- `ParseAffixes`, `ParseModular`, `ParseBias`, `ParseRegister`,
  `ParseCarrier`, `ParseCc`, `ParseSpecialVv`, `ParseAffixVr`
- `ClassifyAffixVowel(vowel) (AffixType, degree)`
- `ShortcutVariant`, `ShortcutCa`, `ShortcutNone`, `VvSeries`
- `IsValidCn`, `IsPattern2Cn`, `IsSpecialVv`
- `NormalizeAccents(s) string` — strip every accent (legacy
  utility used by some lookups; partly redundant with
  `surface.Strip` and could be consolidated)
- `Stress` is now a type alias for `surface.Stress`, kept so older
  signatures in `fullparse` still compile against the familiar name.

## What's not separated yet

`fullparse.ParseFormative` runs Layers C, D, and E together. The
function does shape detection (consonant-initial / vowel-initial /
shortcut / special-Vv), assigns conjuncts to slots, decodes each
slot's bytes to grammar values, and chooses the Final variant based
on Stress — all in one pass.

Likewise `render.Formative` runs E → D → C → B → A in reverse, but
internally entangled.

`inspect.Segments` and `inspect.SegmentsModular` re-do the shape
detection that `fullparse` already did, because there's no shared
"slot layout" intermediate to consume.

The natural Layer C refactor: introduce a `layout` package with a
`Layout` struct that records the slot positions of a surface word
(SentenceStarter, Concat, Shortcut, Vv, Cr/Cs/C1, Vr, SlotV
affixes, Ca, SlotVII affixes, Vn, Cn, Vc, Stress, plus a Kind
discriminator for Cr/Cs/RefRoot). Then:

- `layout.Parse(word) (Layout, error)` — Layer C
- `layout.Render(Layout) string` — inverse
- `layout.FromGrammar(Formative) Layout` — convert decoded grammar back to layout (for renderers)
- `layout.ToGrammar(Layout) (Formative, error)` — Layer D

Both `fullparse.ParseFormative` and `render.Formative` would become
thin compositions over the layout layer. `inspect.Segments` would
consume `Layout` directly and stop duplicating pattern matching.

This is a substantial refactor (probably 4–6 commits) and is best
done after a checkpoint, not in a single sitting.
