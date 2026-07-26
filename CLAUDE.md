# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

Enter the dev shell first, then use `go` directly:

```bash
nix develop              # Provides go and python3 on PATH with CGO_ENABLED=0
python3 data/build_db.py # Build the data store (needed before the CLI runs)
go install ./cmd/...     # Drop binaries into $GOBIN
go build ./...           # Typecheck and compile everything; writes no binaries
go test ./...            # Run the full test suite
```

`CGO_ENABLED=0` is set in `flake.nix` because `segmentio/asm` (transitive from the MCP SDK) tries to invoke gcc otherwise; the pure-Go fallback is used.

Nothing generated belongs in the source tree. Use `go install`, never
`go build ./cmd/ithkuil`, which would drop a binary at the repo root. The
`.gitignore` is one line, and should stay that way: new generated output
goes to `$XDG_DATA_HOME/ithkuil/` (see `store.DefaultPath`), to a path given
on the command line, or to `local/` for anything local-only.

## What This Project Is

A Go implementation of the Ithkuil V4 ("Malëuţřait") constructed language grammar (v1.3.1, 2023). The system parses formatives (words) into their morphological slots, looks up roots in a JSON lexicon, and renders glosses. There is also an MCP server that exposes the same functionality to AI assistants.

The canonical test word is the language's name for itself: "Malëuţřait".

## Architecture

Go packages at the repo root:

- `phonology/` - 31 consonants, 9 vowels, vowel form table (4 series x 9 forms)
- `grammar/` - All morphological types. `Formative` is the central struct (Concat, Root, SlotV, SlotVI, SlotVII, SlotVIII, Final, SentenceStarter). `Root` and `Final` are sum-type interfaces; see `root.go` and `final.go`.
- `surface/` - Pure rune-level work, no grammatical knowledge: `Strip`/`Apply` for stress diacritics, `SplitConjuncts`/`JoinConjuncts` for vowel/consonant runs, `MergeGlottalVowels`, vowel classification. Also `InputState` and `FromASCII`/`ToASCII` for the digraph notation.
- `slots/` - `Layout` is the slot-labelled surface form: one raw conjunct per string field (Cc, Vv, Cr, Vr, Ca, Vn, Cn, Vc), plus affix pairs and observed stress. `Parse`/`Render` convert between surface text and `Layout` by shape alone; `ToGrammar`/`FromGrammar` translate `Layout` ↔ `Formative` through the lookup tables. All canonical-form choices (which shortcut wins, moved-glottal, default elisions) live in `FromGrammar`.
- `parse/` - Grammatical decoders for individual slot positions, plus all lookup tables (Vv, Vr, Vc, Vn, Cn). Nothing text-level.
- `allomorph/` - Slot VI Ca complex construction and parsing. Pre-generates all Ca forms from component tables with allomorphic substitutions, stores bidirectional lookup.
- `semantics/` - Context-dependent labels derived from grammar values: Mood vs CaseScope, V_N vs V_H, the Vn category for a given Cn. Never looks at surface text.
- `fullparse/` - Turns surface text into a `grammar.Formative` (handles stress detection, returns errors). This is `slots.Parse` ∘ `slots.ToGrammar`.
- `render/` - Renders a `grammar.Formative` back to surface text: `slots.FromGrammar` ∘ `slots.Render`.
- `serialize/` - Compact binary encoding of parsed tokens.
- `gloss/` - Human-readable morphological glossing.
- `validation/` - Phonotactic constraint checking (cluster lengths, vowel sequences, stress).
- `tokenize/` - Classifies words in a sentence into formatives, referentials, bias adjuncts, etc.
- `concatenation/` - Type 1/2 compound formative chains.
- `referentials/` - Anaphoric references (11 referent categories x 3 effects, combinations).
- `numbers/` - Centesimal/base-100 number system.
- `compose/` - Builds formatives from grammatical specifications + lexicon search helpers.
- `view/` - Presentation layer for parsed tokens: the per-token type tag (`view.Type`) plus the phonetic-segment + glossary breakdown (`view.Segments`, `view.Headword`, `view.Glossary`) consumed by the analyze CLI and MCP server.
- `store/` - Read-only SQLite access to `data/data.db` (roots, affixes, grammar tables).
- `lexicon/` - Roots and affixes in memory. `LoadFromStore(*store.Store)` is the normal path; `Load(path)` reads the JSON source directly (used by tests).

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
- Data comes from the store at `store.DefaultPath()`; pass `--data FILE` on the CLI to point elsewhere.
- Reference implementations in `local/reference/`: `IthkuilGloss/` (Kotlin), `mamkait/` (Haskell).

## Data Files

- `data/data.json` - Source of truth for the store: roots, affixes, and grammar tables.
- `data/build_db.py` - Builds the SQLite store from `data.json`. Writes to `$XDG_DATA_HOME/ithkuil/data.db` (`~/.local/share/ithkuil/data.db` when unset), which is what `store.DefaultPath()` returns in Go. `-o PATH` overrides.
- `data/sync_lexicon.py` - Refreshes the roots/affixes sections of `data.json` (and the TSV mirrors kept for diff visibility) from the upstream community spreadsheet.
- `discord_archive/` - Scrapers for the community Discord. Output goes to `$XDG_DATA_HOME/ithkuil/discord/`; see `discord_archive/paths.py`.

## Grammar Reference

- `grammar_reference/morphology.md` - Canonical V4 grammar reference (phonology, morphology, slots, cases, adjuncts, syntax, script, numbers)
- `grammar_reference/affixes_reference.md` - All 527 affixes with gradient types and 9 degrees
- `grammar_reference/phonotactics.md` - Detailed consonant cluster rules
- `grammar_reference/v3_script.md`, `v4_script.md`, `v4_script_pdf.md` - Writing system reference
- `local/grammar_reference_pdf/` - Source PDFs, not committed. Put any intermediate extraction output (html, per-page pdf) under `local/` too.

The Ithkuil writing system is documented but not implemented on `main`. The
earlier Python attempts and the extracted reference figures they were checked
against live on the `writing` branch.
