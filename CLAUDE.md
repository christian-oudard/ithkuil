# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

Everything runs inside the dev shell. It is the only supported environment:
`go`, `python3` and `curl` are on PATH there and nowhere else, and it sets
`CGO_ENABLED=0`.

All Go work happens in `code/`, which holds the module; the `go` commands
below only work from there.

```bash
nix develop               # Enter the shell, then run the commands below
python3 tools/build_db.py # Build the data store (needed before the CLI runs)
cd code                   # The Go module lives here, not at the repo root
go install ./cmd/...      # Drop binaries into $GOBIN
go build ./...            # Typecheck and compile everything; writes no binaries
go test ./...             # Run the full test suite
tools/test.sh             # Test suite with cross-package coverage summary (run from anywhere)
```

One-shot form, for anything that can't hold an interactive shell open:

```bash
nix develop --command sh -c 'cd code && go test ./...'
nix develop --command python3 tools/build_db.py
```

Do not reach around the shell: no hand-set `CGO_ENABLED`, no `go` from a Nix
store path, no `nix shell nixpkgs#python3`. Those drift from what `flake.nix`
pins and hide breakage. `CGO_ENABLED=0` is set there because `segmentio/asm`
(transitive from the MCP SDK) otherwise tries to invoke gcc; the pure-Go
fallback is used instead.

The first `nix develop` on a cold store fetches the Go toolchain and can take
several minutes. Let it finish rather than working around it; every later
invocation is instant.

Rebuild the store with `tools/build_db.py` after any edit to `data/data.json`,
or the CLI and the store-backed tests keep reading the old data.

There is no `.gitignore`, and the repo should stay that way. Everything in the
tree is source; everything else lives elsewhere. Generated output goes to
`$XDG_DATA_HOME/ithkuil/` (see `store.DefaultPath`) or to a path given on the
command line, and downloaded reference material goes there too. Use
`go install`, never `go build ./cmd/ithkuil`, which would drop a binary in
`code/`.

## What This Project Is

A Go implementation of the Ithkuil V4 constructed language grammar (v1.3.1, 2023). The system parses formatives (words) into their morphological slots, looks up roots in a JSON lexicon, and renders glosses. There is also an MCP server that exposes the same functionality to AI assistants.

The canonical test word is "Maţřëullait", the community nickname for v4 (not an official name — Quijada calls every version "Iţkuil"). It replaced an earlier form, "Malëuţřait", which has the same morphemes with the SYS affix in Slot VII instead of Slot V.

## Architecture

The repo has four top-level folders: `code/` (the Go module, and the only
place Go lives), `data/`, `docs/`, and `tools/`. Package paths below are
relative to `code/`.

- `phonology/` - §1 of the grammar, sounds and the letters that write them. `inventory.go` holds 31 consonants, 9 vowels, and the vowel form table (4 series x 9 forms); the rest is rune-level work with no grammatical knowledge: `Strip`/`Apply` for stress diacritics, `SplitConjuncts`/`JoinConjuncts` for vowel/consonant runs, `MergeGlottalVowels`, `Normalize`, vowel classification, and `InputState` + `FromASCII`/`ToASCII` for the digraph notation. `ParseWord` is the only constructor of `Word`, which carries the reading (normalized text, stress, conjuncts) that every later layer builds on; the §2 phonotactic rules are a separate judgment on a word already read (`Word.Violations`, `CheckText`, `Legal`), because the Ca tables generate a few clusters our reading of §2 rejects and a parser that refused them could not round-trip its own output.
- `grammar/` - All morphological types, one per word class. `Formative` is the largest (Concat, Root, SlotV, SlotVI, SlotVII, SlotVIII, Final); `Referential` and `CombinationReferential` cover §4.6, and `Bias`, `Register`, `ModularAdjunct`, `CarrierAdjunct`, `ParsingAdjunct` and the two affixual adjuncts the rest. Variation within a class is a sealed sum type: `Root`, `Final`, `SlotVIII`, `Vk`, `RefHead`.
- `slots/` - `Layout` is the slot-labelled romanization: one raw conjunct per string field (Cc, Vv, Cr, Vr, Ca, Vn, Cn, Vc), plus affix pairs and observed stress. `Parse`/`Render` convert between romanization and `Layout` by shape alone; `ToGrammar`/`FromGrammar` translate `Layout` ↔ `Formative` through the lookup tables. All canonical-form choices (which shortcut wins, moved-glottal, default elisions) live in `FromGrammar`.
- `parse/` - Grammatical decoders for individual slot positions, plus all lookup tables (Vv, Vr, Vc, Vn, Cn). Nothing text-level.
- `allomorph/` - Slot VI Ca complex construction and parsing. Pre-generates all Ca forms from component tables with allomorphic substitutions, stores bidirectional lookup.
- `semantics/` - Context-dependent labels derived from grammar values: Mood vs CaseScope, V_N vs V_H, the Vn category for a given Cn. Never looks at the romanization.
- `fullparse/` - Turns a romanization into a grammar value, handling stress and returning errors. `Formative` is `slots.Parse` ∘ `slots.ToGrammar`; `Referential` and `CombinationReferential` decode §4.6 and run the phonotactic checks, so a word the validator rejects is not classified as one.
- `render/` - Renders a grammar value back to a romanization. `Formative` is `slots.FromGrammar` ∘ `slots.Render`; `Referential` and `CombinationReferential` mirror the fullparse entry points. `tokenize.Render` dispatches over the whole word-class sum (it lives there, not here, because the sum type does).
- `serialize/` - Binary encoding of parsed tokens. Default-eliding and
  byte-aligned; no lexicon indices, so files outlive lexicon updates.
  `formative.go` documents why the layout is shaped the way it is.
- `gloss/` - Human-readable morphological glossing.
- `tokenize/` - Classifies words in a sentence into formatives, referentials, bias adjuncts, etc. Each `WordToken` variant is a thin `{Text, payload}` wrapper over the grammar type for its class; `Text` records what was typed and is empty on a synthesized token, so derive the romanization with `tokenize.Render` rather than reading it.
- `concatenation/` - Type 1/2 compound formative chains.
- `numbers/` - Centesimal/base-100 number system.
- `compose/` - Builds formatives from grammatical specifications + lexicon search helpers.
- `view/` - Presentation layer for parsed tokens: the per-token type tag (`view.Type`), the phonetic-segment + glossary breakdown (`view.Segments`, `view.Headword`, `view.Glossary`), and the two-word comparison model (`view.BuildSide`, `view.PairSides`, `view.SlotDiff`, `view.GlossDiff`). Both the CLI and the MCP server build on it; only the table drawing lives in `cmd/ithkuil/compare.go`.
- `store/` - Read-only SQLite access to `data/data.db` (roots, affixes, grammar tables).
- `lexicon/` - Roots and affixes in memory. `LoadFromStore(*store.Store)` is the normal path; `Load(path)` reads the JSON source directly (used by tests).
- `dictionary/` - The English index: reads the lexicon's English glosses backwards into a headword-to-lexical-core map. `english_doc_test.go` checks every claim made in `docs/dictionary/english.md` by composing it.
- `corpus/` - The 384 example sentences published on ithkuil.net, with Quijada's English translations, embedded as test data. Their section numbers follow the site's chapters rather than the Grammar Design PDF, and most do not appear in it; see the head of `examples.txt` before citing one as a passage of the grammar. `corpus.Examples()` and `corpus.Words()`. `tokenize/corpus_test.go` guards the set of words we still fail to classify.
  `discord_examples.txt` + `corpus.DiscordExamples()` hold curated words from the community Discord archive, each marked `correct` or `incorrect` with the rule it rests on. The archive is usage, not authority, so a word cited as evidence should appear there first. A leading `!` marks a word we currently disagree with (a filed defect). `fullparse/discord_examples_test.go` checks we agree.

Command-line entrypoints under `cmd/`:

- `cmd/ithkuil/` - The main CLI. Subcommands: parse, compare, compose, search, define. `main.go` dispatches; `flags.go` parses shared flags, accepting them in any position.
- `cmd/ithkuil-mcp/` - Model Context Protocol server exposing the parser/glosser/lexicon as MCP tools and resources.
- `cmd/ithkuil-input/` - Raw-mode TUI that types Ithkuil Unicode from ASCII digraphs (aa→ä, t,→ţ, sq→š, dz→ẓ). Pending chars are shown dim. Backed by `phonology.InputState`; `phonology.FromASCII` / `ToASCII` provide the batch transforms.

## Key Conventions

- `Formative` lives in `grammar/formative.go`. Invariants: `Root` and `Final` must be non-nil; the zero value is not valid. Use `MinimalFormative(cluster)` as a starting point. `render` and `gloss` panic on nil Root or Final.
- `Root` is a sum-type interface with three variants (`CrRoot`, `CsRoot`, `RefRoot`); it consolidates the lexical identity that the spec splits across Slots II/III/IV.
- A referential is not a kind of formative. §4.6 makes it its own word class, so `grammar.Referential` is a peer of `Formative`, not a `Root` variant. Do not confuse it with `RefRoot`, which is a *formative* whose root is a personal reference (§5.3).
- `Final` is a sum-type interface covering the various case/illocution endings (UnframedNominal, UnframedVerbal, FramedVerbal, etc.).
- `Affix` stores `(Type, Degree)` plus the consonant cluster; never the vowel as written.
- Grammatical values use standard Ithkuil abbreviations (3-letter uppercase): THM, INS, ABS, STA, DYN, BSC, CTE, etc.
- Data comes from the store at `store.DefaultPath()`; pass `--data FILE` on the CLI to point elsewhere.
- Reference implementations, cloned outside the repo to `$XDG_DATA_HOME/ithkuil/reference/`: `IthkuilGloss/` (Kotlin).

## Data Files

- `data/data.json` - Source of truth for the store: roots, affixes, and grammar tables.
- `data/gen_grammar.go` - `//go:build ignore` one-shot that printed the grammar section of `data.json`. Never compiled by `go build`, so assume it has rotted.

## Tools

Everything under `tools/` is non-Go tooling. Go tools stay with the code they belong to.

- `tools/build_db.py` - Builds the SQLite store from `data/data.json`. Writes to `$XDG_DATA_HOME/ithkuil/data.db` (`~/.local/share/ithkuil/data.db` when unset), which is what `store.DefaultPath()` returns in Go. `-o PATH` overrides.
- `tools/sync_lexicon.py` - Refreshes the roots/affixes sections of `data.json` (and the TSV mirrors kept for diff visibility) from the upstream community spreadsheet.
- `tools/test.sh` - Go test suite with a cross-package coverage summary. `COVERAGE_THRESHOLD=NN` fails below a floor; `SHOW_UNCOVERED=1` lists functions under 100%.
- `tools/discord_archive/` - Scrapers for the community Discord. Output goes to `$XDG_DATA_HOME/ithkuil/discord/`; see its `paths.py`.

## Documents

All prose lives under `docs/`, and no Go code does. `docs/reference/`
holds the language reference below; `docs/dictionary/` holds one file per
natural language (`english.md` so far) mapping that language into
Ithkuil, authored rather than derived — see the "English index" section
of SPEC.md, and the head of `english.md` for what earns an entry.
`README.md`, `SPEC.md`, and this file stay at the root by convention.

- `docs/reference/morphology.md` - Canonical V4 grammar reference (phonology, morphology, slots, cases, adjuncts, syntax, numbers)
- `docs/reference/affixes_reference.md` - All 528 affixes with gradient types and 9 degrees
- `docs/reference/phonotactics.md` - Detailed consonant cluster rules
- `docs/reference/ISSUES.md` - Defects in the published sources, not in this code.
- `docs/reference/source_versions.md` - Quijada's published version history for the three documents. The documents themselves carry only the current version.
- Source PDFs and any intermediate extraction output (html, per-page pdf) go outside the repo, under `$XDG_DATA_HOME/ithkuil/reference/`.

`morphology.md` and `phonotactics.md` are transcriptions of Quijada's
PDFs and should stay faithful to them, including where a source is
wrong; record the defect in `ISSUES.md` rather than silently correcting
the transcription. `affixes_reference.md` is not a transcription: it
tracks `data/data.json`, which merges the community spreadsheet with the
affix document.

The writing system is not covered on `main` at all, neither implementation nor
reference material: `v4_script.md`, `v4_script_pdf.md`, the extracted figures,
and the earlier Python attempts are all on the `writing` branch, bound for their
own repository. Quijada's script document is separate from the grammar document,
so nothing here has to carry it to keep a document whole.

## Open Work

`BUGS.md` indexes the open defects in this code. It holds a pointer per bug and
nothing more: the record itself is a skipped test next to the code it concerns,
carrying the section it rests on and why the obvious fix is wrong, and
`go test ./... -v | grep SKIP` lists those directly. Words we cannot read live
in the drift guards (`tokenize/corpus_test.go`, `corpus/discord_examples.txt`),
which fail when the set changes in either direction. Defects in the published
sources go in `docs/reference/ISSUES.md`.
