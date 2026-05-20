# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

Enter the dev shell first, then use `go` directly:

```bash
nix-shell                # Provides go on PATH with CGO_ENABLED=0
go install ./cmd/ithkuil ./cmd/ithkuil-mcp ./cmd/ithkuil-input   # Drop binaries into $GOBIN
go build ./...           # Build everything in place
go test ./...            # Run the full test suite
```

`CGO_ENABLED=0` is set in `shell.nix` because `segmentio/asm` (transitive from the MCP SDK) tries to invoke gcc otherwise; the pure-Go fallback is used.

## What This Project Is

A Go implementation of the Ithkuil V4 ("Malëuţřait") constructed language grammar (v1.3.1, 2023). The system parses formatives (words) into their morphological slots, looks up roots in a JSON lexicon, and renders glosses. There is also an MCP server that exposes the same functionality to AI assistants.

The canonical test word is the language's name for itself: "Malëuţřait".

## Architecture

Go packages at the repo root:

- `phonology/` - 31 consonants, 9 vowels, vowel form table (4 series x 9 forms)
- `grammar/` - All morphological types. `Formative` is the central struct (Concat, Root, SlotV, SlotVI, SlotVII, SlotVIII, Final, SentenceStarter). `Root` and `Final` are sum-type interfaces; see `root.go` and `final.go`.
- `parse/` - Conjunct splitting and slot-by-slot parsing primitives. Contains all lookup tables (Vv, Vr, Vc, Vn, Cn).
- `allomorph/` - Slot VI Ca complex construction and parsing. Pre-generates all Ca forms from component tables with allomorphic substitutions, stores bidirectional lookup.
- `fullparse/` - Turns surface text into a `grammar.Formative` (handles stress detection, returns errors).
- `render/` - Renders a `grammar.Formative` back to surface text. All encoding decisions (shortcut form, default-value elision, stress placement, etc.) live here, not in `grammar`.
- `gloss/` - Human-readable morphological glossing.
- `validation/` - Phonotactic constraint checking (cluster lengths, vowel sequences, stress).
- `tokenize/` - Classifies words in a sentence into formatives, referentials, bias adjuncts, etc.
- `concatenation/` - Type 1/2 compound formative chains.
- `referentials/` - Anaphoric references (11 referent categories x 3 effects, combinations).
- `numbers/` - Centesimal/base-100 number system.
- `compose/` - Builds formatives from grammatical specifications + lexicon search helpers.
- `view/` - Presentation layer for parsed tokens: the per-token type tag (`view.Type`) plus the phonetic-segment + glossary breakdown (`view.Segments`, `view.Headword`, `view.Glossary`) consumed by the analyze CLI and MCP server.
- `lexicon/` - Loads roots and affixes from JSON; `LoadDefault()` returns the embedded lexicon.

Command-line entrypoints under `cmd/`:

- `cmd/ithkuil/` - The main CLI. Subcommands: analyze, compose, diff, grammar, lexicon, validate. `main.go` dispatches; `flags.go` parses shared flags.
- `cmd/ithkuil-mcp/` - Model Context Protocol server exposing the parser/glosser/lexicon as MCP tools and resources.
- `cmd/ithkuil-input/` - Raw-mode TUI that types Ithkuil Unicode from ASCII digraphs (aa→ä, t,→ţ, sq→š, dz→ẓ). Pending chars are shown dim. Backed by `surface.InputState`; `surface.FromASCII` / `ToASCII` provide the batch transforms.

## Key Conventions

- `Formative` lives in `grammar/formative.go`. Invariants: `Root` and `Final` must be non-nil; the zero value is not valid. Use `MinimalFormative(cluster)` as a starting point. `render` and `gloss` panic on nil Root or Final.
- `Root` is a sum-type interface with three variants (`CrRoot`, `CsRoot`, `RefRoot`); it consolidates the lexical identity that the spec splits across Slots II/III/IV.
- `Final` is a sum-type interface covering the various case/illocution endings (UnframedNominal, UnframedVerbal, FramedVerbal, etc.).
- `Affix` stores `(Type, Degree)` plus the consonant cluster; never the surface vowel string.
- Grammatical values use standard Ithkuil abbreviations (3-letter uppercase): THM, INS, ABS, STA, DYN, BSC, CTE, etc.
- `lexicon.LoadDefault()` returns the embedded lexicon; pass `-lex DIR` on the CLI to override with a local copy.
- Reference implementations in `reference/` (gitignored): `IthkuilGloss/` (Kotlin), `mamkait/` (Haskell).

## Data Files

- `data/roots.json` / `data/affixes.json` - Lexicon, also embedded into the binaries via `//go:embed`.
- `data/convert_lexicon.py` / `data/update_affixes.py` - Maintenance scripts.

## Grammar Reference

- `grammar_reference/morphology.md` - Canonical V4 grammar reference (phonology, morphology, slots, cases, adjuncts, syntax, script, numbers)
- `grammar_reference/affixes_reference.md` - All 527 affixes with gradient types and 9 degrees
- `grammar_reference/phonotactics.md` - Detailed consonant cluster rules
- `grammar_reference_pdf/` - Gitignored: source PDFs
