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
tools/test.sh             # The gate: suite, coverage, dead-code pass (run from anywhere)
tools/build_wasm.sh       # Browser module, via TinyGo (run from anywhere)
tools/test_tinygo.sh      # Suite under TinyGo, the browser compiler (run from anywhere)
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

The repo has five top-level folders: `code/` (the Go module, and the only
place Go lives), `data/`, `docs/`, `tools/`, and `web/`. Package paths below are
relative to `code/`.

- `phonology/` - §1 of the grammar, sounds and the letters that write them. `inventory.go` holds 31 consonants, 9 vowels, and the vowel form table (4 series x 9 forms); the rest is rune-level work with no grammatical knowledge: `Strip`/`Apply` for stress diacritics, `SplitConjuncts`/`JoinConjuncts` for vowel/consonant runs, `MergeGlottalVowels`, `Normalize`, vowel classification, and `InputState` + `FromASCII`/`ToASCII` for the digraph notation. `ParseWord` is the only constructor of `Word`, which carries the reading (normalized text, stress, conjuncts) that every later layer builds on; the §2 phonotactic rules are a separate judgment on a word already read (`Word.Violations`, `CheckText`, `Legal`), because the Ca tables generate a few clusters our reading of §2 rejects and a parser that refused them could not round-trip its own output.
- `grammar/` - All morphological types, one per word class. `Formative` is the largest (Concat, Root, SlotV, SlotVI, SlotVII, SlotVIII, Final); `Referential` and `CombinationReferential` cover §4.6, and `Bias`, `Register`, `ModularAdjunct`, `CarrierAdjunct`, `ParsingAdjunct` and the two affixual adjuncts the rest. Variation within a class is a sealed sum type: `Root`, `Final`, `SlotVIII`, `Vk`, `RefHead`.
- `slots/` - `Layout` is the slot-labelled romanization: one raw conjunct per string field (Cc, Vv, Cr, Vr, Ca, Vn, Cn, Vc), plus affix pairs and observed stress. `Parse`/`Render` convert between romanization and `Layout` by shape alone; `ToGrammar`/`FromGrammar` translate `Layout` ↔ `Formative` through the lookup tables. All citation-form choices (which shortcut wins, moved-glottal, default elisions) live in `FromGrammar`. `Spellings` is the other half of that: `FromGrammar` picks the shortest spelling and `Spellings` returns all of them, which is what a caller with a neighbour to consider needs. The slack is exactly five decisions (the §3.2 Ca shortcut, the §3.8.1.2 C_N-into-Ca move, the §3.9.1 moved glottal, and the Slot II and Slot IX defaults), and four corpus formatives in five have more than one legal spelling. Every candidate is checked both ways, legal to say and parsing back to an equal `Formative`.
- `parse/` - Grammatical decoders for individual slot positions, plus all lookup tables (Vv, Vr, Vc, Vn, Cn). Nothing text-level.
- `allomorph/` - Slot VI Ca complex construction and parsing. Pre-generates all Ca forms from component tables with allomorphic substitutions, stores bidirectional lookup.
- `semantics/` - Context-dependent labels derived from grammar values: Mood vs CaseScope, V_N vs V_H, the Vn category for a given Cn. Never looks at the romanization.
- `roman/` - The romanization arm, both directions in one package, because they encode one thing — which letters spell which grammar — and splitting them by direction is what lets them drift. Reading takes a `Parse` prefix and writing keeps the noun: `ParseFormative`/`Formative`, `ParseReferential`/`Referential`, `ParseWord`/`Word`, `ParseText`/`Text`. `Tokenize` is the per-word report, pairing each romanization with the word it produced or the reason there is none. `Word` writes one word in citation form, the shortest spelling; `Text` writes running text, choosing each word's spelling in the context of the next. That difference is §1.5: a word with another word after it should end in a vowel, and since §1.2 makes every word consonant-initial the rule's antecedent holds at every junction. `Text` also declines a word-final bare `-h` anywhere, and treats a §3.1.7 chain link as a junction, since §3.1.8 calls the hyphen a mnemonic indicator and nothing is paused there. `Stressless` writes stress as a §4.8 parsing adjunct instead of a diacritic. Absorbed `fullparse`, `render` and `tokenize`.
- `serialize/` - Binary encoding of parsed tokens. Default-eliding and
  byte-aligned; no lexicon indices, so files outlive lexicon updates.
  `formative.go` documents why the layout is shaped the way it is.
- `gloss/` - The gloss arm, both directions in one package. `Formative`/`Word`/`Text` write a gloss; `ParseFormative`/`ParseWord`/`ParseText` read one. They belong together because they encode one syntax, and while they were split it drifted — the glosser emitted `NOM:1m` for a §4.6 referent category the parser had no rule for.
- `concatenation/` - Type 1/2 compound formative chains.
- `numbers/` - Centesimal/base-100 number system.
- `search/` - Reverse lookup over the grammar inventory and the lexicon: by abbreviation, written form, or meaning keyword. Backs the `search` subcommand, and was in `compose` only because that subcommand grew around it.
- `view/` - Presentation layer for parsed tokens: the per-token type tag (`view.Type`), the phonetic-segment + glossary breakdown (`view.Segments`, `view.Headword`, `view.Glossary`), and the two-word comparison model (`view.BuildSide`, `view.PairSides`, `view.SlotDiff`, `view.GlossDiff`). Both the CLI and the MCP server build on it; only the table drawing lives in `cmd/ithkuil/compare.go`.
- `api/` - The one layer all three front ends read: the CLI, the MCP server and the browser module. It holds the orchestration (tokenize, gloss, break down, look up) and the shapes, with explicit json tags so no Go field name reaches a caller by accident.  The MCP server used to declare seventeen near-copies of types in `view` and `lexicon` and map them across by hand; it declares none now. `SetLexicon`, `SetNotes` and `SetLexiconSearch` are how a store-backed caller hands in what it read, since this package must stay linkable for js/wasm and so cannot import `store`. The lexicon half of a search is injected for the same reason: SQLite's full-text index ranks by word and the in-memory scan matches substrings, and the CLI should not be downgraded to what a browser can manage. Every type carries explicit json tags, so no Go field name reaches the wire by accident, and the types are close to the internal ones but deliberately not the same (a root's four stems become an array, Wikidata Q-IDs are dropped). Builds on every platform, so the normal suite covers it and an HTTP server could serve the same shapes later. `web/ithkuil.d.ts` is the other half, and `api/dts_test.go` fails when they disagree.
- `store/` - Read-only SQLite access to `data/data.db` (roots, affixes, grammar tables). `LoadLexicon` reads the whole thing into a `lexicon.Lexicon`.
- `lexicon/` - Roots and affixes in memory. `store.LoadLexicon(*store.Store)` is the normal path; `Load(path)` reads the JSON source from disk and `Parse(bytes)` from memory. The dependency runs store -> lexicon and not the other way, which is what keeps the SQLite driver out of every package that only wants a root's meaning: the driver has no `js/wasm` build, so a browser could not link `lexicon` at all if it imported `store`.
- `dictionary/` - The English index: reads the lexicon's English glosses backwards into a headword-to-lexical-core map. `english_doc_test.go` checks every claim made in `docs/dictionary/english.md` by composing it.
- `corpus/` - The 384 example sentences published on ithkuil.net, with Quijada's English translations, embedded as test data. Their section numbers follow the site's chapters rather than the Grammar Design PDF, and most do not appear in it; see the head of `examples.txt` before citing one as a passage of the grammar. `corpus.Examples()` and `corpus.Words()`. `roman/corpus_test.go` guards the set of words we still fail to classify.
  `corpus.DiscordExamples()` reads curated words from the community Discord archive, each marked `correct` or `incorrect` with the rule it rests on. The archive is usage, not authority, so a word cited as evidence should appear there first. A leading `!` marks a word we currently disagree with (a filed defect). `roman/discord_examples_test.go` checks we agree, and skips where the list is absent. The list is not in the repo: its words are other people's chat messages rather than published grammar, so it is a testing record beside the mirror it came from, at `corpus.DiscordExamplesPath()`.
- `inventory/` - The other half of the test material, and the complement of `corpus/`: one minimal word per grammatical value, differing from a fixed baseline in that value alone. A corpus can only show what people happen to say, so `roman/inventory_test.go` and `gloss/inventory_test.go` sweep this instead to check each arm over the whole grammar. `inventory`'s own test holds the sample set against `search.Table`, whose test holds that against the store, so "every value" is checked at each link. `Samples` and `Pairs` both read one list of `Axes`, so they cannot come to disagree about what the inventory holds; `Pairs` crosses two axes at a time and skips the ones that interfere, deciding that by whether the two edits commute rather than by a list someone keeps current. It carries one axis `Samples` does not, the slot an affix sits in, which is a structural choice no published table holds a value for and the one a sweep over values alone cannot reach.

Command-line entrypoints under `cmd/`:

- `cmd/ithkuil/` - The main CLI. Subcommands: parse, compare, compose, search, define. `main.go` dispatches; `flags.go` parses shared flags, accepting them in any position.
- `cmd/ithkuil-mcp/` - Model Context Protocol server exposing the parser/glosser/lexicon as MCP tools and resources.
- `cmd/ithkuil-wasm/` - Binds `api` to `globalThis.ithkuil`, `//go:build js && wasm` so the host toolchain skips it. Deliberately thin: it converts `js.Value` to Go arguments and hands back `api.Reply`, and decides nothing, so nothing in it can drift from what the tests check. Built by `tools/build_wasm.sh`, never by `go build`.
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

- `data/data.json` - Source of truth for the store: roots, affixes, grammar tables, and topics.
  Each grammar value may carry an `explanation` (a fuller reading than the
  one-line `description`) and `guidance` (how it lands in English). Both are
  authored rather than transcribed, unlike everything around them: they were
  written by running a gloss through a model, comparing against Quijada's own
  English, and recording what the model got wrong. That is why so many read as
  corrections. 160 of the 294 values have them; a value with nothing surprising
  about it has neither, which is not an oversight. `topics` holds the 42
  explanations belonging to no single value: a construction, a slot, an affix
  pattern, or a value read in a second context (an illocution carried by a Vk
  affix rather than a slot). Descriptions are Quijada's, under the reuse terms
  published on ithkuil.net; the guidance came from the IthkuilTranslator
  project, merged with its author's permission.
- `data/gen_grammar.go` - `//go:build ignore` one-shot that printed the grammar section of `data.json`. Never compiled by `go build`, so assume it has rotted.

## Tools

Everything under `tools/` is non-Go tooling. Go tools stay with the code they belong to.

- `tools/build_db.py` - Builds the SQLite store from `data/data.json`. Writes to `$XDG_DATA_HOME/ithkuil/data.db` (`~/.local/share/ithkuil/data.db` when unset), which is what `store.DefaultPath()` returns in Go. `-o PATH` overrides.
- `tools/sync_lexicon.py` - Refreshes the roots/affixes sections of `data.json` (and the TSV mirrors kept for diff visibility) from the upstream community spreadsheet.
- `tools/test_tinygo.sh` - Runs the suite under TinyGo, which compiles the browser build and which the standard suite does not cover: it has its own runtime, GC and standard library. It caught a live panic the standard toolchain hid. Skips four packages, all for reasons that do not touch shipped code; the script's head says which and why.
- `tools/build_wasm.sh` - Builds `cmd/ithkuil-wasm` with TinyGo, runs `wasm-opt -Oz`, copies TinyGo's `wasm_exec.js` (not interchangeable with the standard toolchain's) and `data.json`, and prints raw and Brotli sizes. Output goes to `$XDG_DATA_HOME/ithkuil/web` or to a path given as `$1`. Its head records every size measurement behind the toolchain choice, including the two size cuts that were measured and refused.
- `tools/test.sh` - The test gate; see the Testing section for what it checks and why. `COVERAGE_THRESHOLD=NN` fails below a floor, `SHOW_UNCOVERED=1` lists functions under 100%, `SKIP_DEADCODE=1` skips the reachability pass. `deadcode` is pinned by the `tool` directive in `code/go.mod`.
- `tools/discord_archive/` - Scrapers for the community Discord. Output goes to `$XDG_DATA_HOME/ithkuil/discord/`; see its `paths.py`.

## Documents

All prose lives under `docs/`, and no Go code does. `docs/reference/`
holds the language reference below; `docs/dictionary/` holds one file per
natural language (`english.md` so far) mapping that language into
Ithkuil, authored rather than derived — see the "English index" section
of SPEC.md, and the head of `english.md` for what earns an entry.
`README.md`, `SPEC.md`, and this file stay at the root by convention.

`docs/web_interface.md` is mostly a design rather than a record: it says
what a website over this repo should do and why, and no page exists yet.
Its central claim is that the browser calls this Go rather than carrying
a second copy of the grammar; `code/api`, `cmd/ithkuil-wasm` and
`web/ithkuil.d.ts` are the part of it that is built.

`web/` holds the front end. So far that is `ithkuil.d.ts`, the
TypeScript half of the `api` contract, hand-written and checked against
the Go by `api/dts_test.go`. That file is the API reference, since it is
the only documentation here a test can fail on; `docs/web_api.md` is the
part a type declaration cannot say, which is what to call in what order
and what works before the lexicon has arrived.

`docs/romanization_design.md` likewise: it is a design document, not a
reference. One `Formative` admits several legal spellings, and it splits
the structural choice from the phonetic one, an explicit form per word
and a spoken form per span, so that the code choosing between legal
spellings cannot reach the code deciding what is legal.

- `docs/reference/morphology.md` - Canonical V4 grammar reference (phonology, morphology, slots, cases, adjuncts, syntax, numbers)
- `docs/reference/affixes_reference.md` - All 528 affixes with gradient types and 9 degrees
- `docs/reference/phonotactics.md` - Detailed consonant cluster rules
- `docs/reference/ERRATA.md` - What Quijada said and what we do instead: corrections
  to the sources, readings where they cannot be implemented as written, and proposed
  amendments. Permanent, and checked against the code by `phonology/errata_test.go`.
- `docs/reference/ISSUES.md` - Worklist of defects in the published sources that we
  have not yet decided about. Drains into `ERRATA.md` and will be deleted at zero.
- `docs/reference/READING.md` - two facts a section number will not tell you: the site is a third document, and the two PDFs collide on §1.5, §1.6, §1.7 and §4.2.
- Source PDFs and any intermediate extraction output (html, per-page pdf) go outside the repo, under `$XDG_DATA_HOME/ithkuil/reference/`.

`morphology.md` and `phonotactics.md` began as transcriptions of
Quijada's PDFs and are being taken further: we correct his errors and
propose improvements. Nothing changes silently. A correction is applied
only alongside an `ERRATA.md` entry giving the source wording, the
change and the reason, so that what he said and what we decided can
always be told apart. An error found but not yet decided about goes on
the `ISSUES.md` worklist. `affixes_reference.md` is not a transcription: it
tracks `data/data.json`, which merges the community spreadsheet with the
affix document.

The writing system is not covered on `main` at all, neither implementation nor
reference material: `v4_script.md`, `v4_script_pdf.md`, the extracted figures,
and the earlier Python attempts are all on the `writing` branch, bound for their
own repository. Quijada's script document is separate from the grammar document,
so nothing here has to carry it to keep a document whole.

## Testing

`tools/test.sh` is the gate, not `go test ./...`. It runs the suite, prints
cross-package coverage, and then fails on any function no main or test can
reach. Running `go test` alone has already let through a commit whose message
claimed a deletion that a stray `git checkout` had undone.

Four checks, each blind to what the others catch:

- **The suite.** `go test ./...`, for correctness.
- **Coverage.** `SHOW_UNCOVERED=1 tools/test.sh` lists what is below 100%. A
  function at 0% is ambiguous — untested, or unreachable — and the two want
  opposite fixes, which is why the next check exists.
- **Reachability.** `go tool deadcode -test ./...`, run by `tools/test.sh`.
  Go makes an unused local a compile error and an unused package-level
  function nothing at all, so a refactor that removes the last caller leaves
  the callee behind in silence. Eight had collected before this pass existed;
  the first rebase after it landed caught two more, orphaned that same day.
- **Skips.** `go test ./... -v | grep SKIP`. A `t.Skip` inside a test body is
  invisible to both coverage and deadcode, and reads as a pass. One had never
  run its assertions at all: it searched a two-word sample for a §3.1.7 chain,
  neither word was one, and it skipped every time.

Before deleting what deadcode reports, find the commit that removed the last
caller. Most are orphans of a named refactor and simply go. Some are half of a
pair the package doc advertises — `roman.Text` is one — and want the test they
never had instead.

Coverage percentage is a floor, not a goal. Every defect this suite has caught
in its own code came from somewhere else: from exhausting an inventory
(`inventory/`, every grammatical value through each arm, which found two
lookup tables disagreeing with the store), from a property a round trip cannot
see (`serialize`'s truncation and corruption sweeps, which found a decoder
that crashed on a corrupt byte), or from reachability. None came from raising
the number.

## Open Work

`BUGS.md` indexes the open defects in this code. It holds a pointer per bug and
nothing more: the record itself is a skipped test next to the code it concerns,
carrying the section it rests on and why the obvious fix is wrong, and
`go test ./... -v | grep SKIP` lists those directly. Not every skip is one:
see Testing for the other kind. Words we cannot read live
in the drift guards (`roman/corpus_test.go`, the Discord word list),
which fail when the set changes in either direction. Defects in the published
sources go in `docs/reference/ISSUES.md`.
