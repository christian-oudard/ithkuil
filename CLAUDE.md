# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

Enter the dev shell first, then use cabal directly:

```bash
nix-shell                # Provides ghc + cabal-install
cabal build              # Build library + executable
cabal test               # Run hspec test suite (test/Main.hs)
cabal run gloss-test     # Run morphological analysis demo (GlossTest.hs)
```

Run `cabal update` on first build to fetch the Hackage index. `-Wall` is enabled project-wide.

## What This Project Is

A Haskell implementation of the Ithkuil constructed language grammar across all four versions (V1 2004, V2 2007, V3 2011, V4 2023). The primary focus is V4 ("Malëuţřait"), with partial implementations of earlier versions. The system parses formatives (words) into their morphological slots, looks up roots in a JSON lexicon, and renders glosses.

The canonical test word for each version is its own name for the language: V1 "Iţkuîl", V2 "ilákš", V3 "Elartkha", V4 "Malëuţřait".

## Architecture

**V4 core modules** (under `src/Ithkuil/`):
- `Phonology.hs` - 31 consonants, 9 vowels, vowel form table (4 series x 9 forms)
- `Grammar.hs` - All morphological types: 68 cases, 20 configurations, stems, versions, Ca complex features. Defines `Formative` with 10 slots (I-IX + stress)
- `Parse.hs` - Conjunct splitting (vowel/consonant sequences), slot-by-slot parsing into `ParsedFormative`. Contains all lookup tables (Vv, Vr, Vc, Vn, Cn)
- `Allomorph.hs` - Ca complex construction and parsing. Pre-generates all 3840 Ca forms from component tables (Ca1-Ca4) with allomorphic substitutions, stores bidirectional lookup
- `FullParse.hs` - Complete parsing with stress detection, uses `ParseResult` monad for error reporting
- `Render.hs` - Formative-to-text rendering, uses Allomorph for Ca and Parse tables for Vr/Vc
- `Gloss.hs` - Human-readable glossing at three precision levels (Short/Regular/Full)
- `Validation.hs` - Phonotactic constraint checking (cluster lengths, vowel sequences)
- `Lexicon.hs` - Loads roots and affixes from `data/roots.json` (4717 entries) and `data/affixes.json`
- `Script.hs` - Morpho-phonemic writing system (Primary/Secondary/Tertiary/Quaternary characters)

**Supplementary V4 modules**: `Concatenation.hs` (Type 1/2 compound formatives), `Adjuncts.hs` (70+ bias markers, registers), `Numbers.hs` (centesimal/base-100 system), `Referentials.hs` (anaphoric references)

**Version-specific modules** (`V1/`, `V2/`, `V3/` subdirs, each with `Grammar.hs` + `Parse.hs`):
- V1: Consonant grades (8 mutations), biliteral roots, primary/secondary mode
- V2: Tonal vowel system (High/Low/Rising/Falling/Neutral)
- V3: Designation (Formal/Informal), Pattern (P1-P3), Sanction (9 evidentiality values)

## Key Conventions

- `type SlotVI = (Configuration, Affiliation, Perspective, Extension, Essence)` - note the order
- Record fields use version-prefixed names: `fSlotII` (V4 Formative), `pfRoot` (ParsedFormative)
- Grammatical values use standard Ithkuil abbreviations (3-letter uppercase): THM, INS, ABS, STA, DYN, BSC, CTE, etc.
- `vowelForm :: Int -> Int -> Text` is the central encoding table mapping grammatical values to vowels
- `splitConjuncts :: Text -> [Text]` is the key parsing primitive that segments words into alternating V/C chunks
- Ca complex is built compositionally: Ca1(config) + Ca2(extension) + Ca3(affiliation) + Ca4(perspective+essence), then allomorphic substitutions applied
- Series 1 Vr skips form 4 (STA/OBJ = form 5); Series 2-4 skip form 5 (STA/OBJ = form 4)
- Reference implementations: `reference/IthkuilGloss/` (Kotlin) and `reference/mamkait/` (Haskell)

## Data Files

- `data/roots.json` / `data/affixes.json` - V4 lexicon (JSON, loaded via aeson)
- `data/roots_v3.json` / `data/roots_v1.json` - Minimal historical lexicons
- `data/convert_lexicon.py` - Converter script for lexicon formats

## Planning Documents

- `PARSER_PLAN.md` - Phase-by-phase implementation plan for all four version parsers
- `COMPARISON.md` - Cross-language (Haskell/OCaml/Lean) implementation comparison
