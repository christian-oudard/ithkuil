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

## Package map

| Layer | Package                                | Status            |
|-------|----------------------------------------|-------------------|
| A     | `surface/` (stress.go)                 | ✅ extracted, round-trip tested |
| B     | `surface/` (conjunct.go)               | ✅ extracted, round-trip tested |
| C     | `slots/` (parse.go + render.go)       | ✅ extracted, round-trip tested |
| D     | `slots/` (grammar.go) on top of `parse/` and `allomorph/` decoders | ✅ extracted, round-trip tested |
| E     | `semantics/`                           | ✅ extracted, unit-tested |

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

## What `slots/` owns

The `slots.Layout` struct is the slot-labelled surface form of a
formative. Every string field carries a raw conjunct (Cc, Vv, Cr,
Vr, Ca, Vn, Cn, Vc) or a list of (Vx, Cs) affix pairs (Slot V,
Slot VII). `Stress` is the surface stress observed (Layer A); a
`Kind` discriminator picks Cr / Cs-root / ref-root.

Layer C — pattern recognition, no grammar decoding:

- `slots.Parse(word) (Layout, error)` — runs Strip + SplitConjuncts
  internally, then assigns each conjunct to its slot based on shape
  (vowel-initial, consonant-initial, shortcut, special-Vv).
- `slots.Render(Layout) string` — the inverse. Re-applies the
  §3.5.1 Vv glottal-stop and §3.6.1 Ca gemination based on Slot V
  presence, then Apply for the stress diacritic.

Layer D — string ↔ grammar value translation:

- `slots.ToGrammar(Layout) (Formative, error)` — looks up each
  slot's string in the grammar tables.
- `slots.FromGrammar(Formative) Layout` — the inverse. Picks
  shortcut yes/no, applies default-value elisions, and emits a
  Layout ready for `Render`.

`fullparse.ParseFormative` is `Parse` ∘ `ToGrammar`.
`render.FormativeWithOpts` is `FromGrammar` ∘ `Render`.
`view.Segments` consumes the Layout directly to emit the
slot-by-slot phonetic breakdown.

Round-trip tests in `slots/roundtrip_test.go` (surface↔Layout)
and `slots/grammar_test.go` (Layout↔Formative) exercise the
inverses against the working corpus.

## What `semantics/` owns

Layer E is pure functions that turn Layer-D values into
context-dependent labels. Nothing in `semantics/` looks at surface
text directly; callers hand it the grammar-side objects plus the
neighbor information already inferred by `tokenize/`.

- `MoodOrCaseScope(mood, isVerbal)` — verbal Mood label vs nominal
  CaseScope twin (§3.8.1).
- `SlotVIIICnLabel(slotVIII, final)` — applies the above to a parsed
  formative's Slot VIII, picking verbal/nominal from `IsVerbal(final)`.
- `IsVH(stress, pairCount)` — modular slot 4 = V_H when ultimate
  stress AND ≥1 (Vn, Cn) pair (§4.3); otherwise V_N.
- `ModularIsVerbal(slotVIII, marksMood)` — modular Cn pattern. When
  `tokenize.MarksMood` set the verbal/nominal flag from a neighbor,
  that wins; otherwise we fall back to the Vn pattern (Pattern-1 →
  Mood, Pattern-2 → CaseScope).
- `VnCategory(vn, cn)` — picks Aspect vs Valence/Phase/Effect/Level
  for a modular Vn based on the paired Cn.
- `CnLabel(cn, asMood)` — modular Cn as Mood or CaseScope (or the
  Cm "n"/"ň" marker codes).
- `VhCode`/`VhMeaning`, `PrefixCode`/`PrefixMeaning`, `CmName`/
  `CmMeaning` — pure prose lookups for the scope-bearing pieces of
  a modular adjunct.

Common visitors over the SlotVIII sum type live in `grammar/` as
`SlotVIIIMoodScope` and `SlotVIIIVnLabel`. `view/` and `gloss/`
use them instead of re-doing the five-case type switch.

`view/` and `gloss/` are the presentation layer — they take parsed
tokens, ask `semantics/` for the context-dependent labels, and render
them into the per-token type tag, phonetic-segment breakdown, or the
hyphen-separated gloss string.
