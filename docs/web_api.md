# The Browser Module

How to use `ithkuil.wasm` from a page. What each call answers with is
declared in `web/ithkuil.d.ts` and checked against the Go by
`code/api/dts_test.go`, so that file is the reference and this one is
the part a type declaration cannot say: what to call, in what order, and
what works before the rest has arrived.

## The files

`tools/build_wasm.sh` writes four, to `$XDG_DATA_HOME/ithkuil/web` or to
a path given as its argument. Brotli'd sizes, which is what reaches a
reader:

    ithkuil.wasm    348 KB    the parser, glosser, composer and tables
    wasm_exec.js     17 KB    TinyGo's loader
    affixes.json     68 KB    528 affixes, and the authored notes
    roots.json      260 KB    5,891 roots

`wasm_exec.js` is TinyGo's and is **not** interchangeable with the
standard toolchain's. They disagree on the import object, and swapping
them fails at instantiation with an unhelpful message. Copy the one the
build script emitted rather than one found elsewhere.

## Booting

```js
await import("./wasm_exec.js");            // defines globalThis.Go
const go = new Go();
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("./ithkuil.wasm"), go.importObject);
go.run(instance);                          // installs globalThis.ithkuil
```

`go.run` does not return; the module parks on an empty `select` so its
callbacks stay alive. Call it and carry on, do not await it.

## The envelope

Every call is synchronous and returns a JSON string holding either
`{"ok": ...}` or `{"error": {"message": ...}}`. Nothing throws. In this
domain a rejected word is an ordinary answer, and the reason belongs
beside the word that produced it rather than in a catch block.

`ithkuil.d.ts` exports the unwrapper:

```js
const call = (raw) => {
  const env = JSON.parse(raw);
  if ("error" in env) throw new Error(env.error.message);
  return env.ok;
};
```

Use it everywhere, so the envelope is opened in one place.

## Loading the lexicon

Parsing needs no lexicon. Only meanings do. So the module answers
structurally from the moment it starts, and the two lexicon files can
arrive whenever they arrive:

```js
call(ithkuil.parse("Maţřëullait"));        // works immediately
call(ithkuil.load(await (await fetch("./affixes.json")).text()));
call(ithkuil.load(await (await fetch("./roots.json")).text()));
```

`load` merges rather than replaces, so a later call carrying only roots
keeps the affixes. What each stage buys:

- **Before either.** Slots, segments, the gloss, phonotactic violations,
  the grammar tables, `positions`, `examples`, `inventory`, `input`.
  Affixes in the gloss show as bare clusters, since an abbreviation is a
  lexicon fact: `m-t,rq/5_2-{Ca}-t/1_2` rather than
  `m-SYS/5_2-{Ca}-DCD/1_2`. Clusters are written in the ASCII digraph
  notation there, `t,rq` for ţř, so that a gloss stays typeable and
  `compose` takes back what `parse` emitted.
- **After `affixes.json`.** Affix abbreviations and degree ladders, and
  the authored `explanation` and `guidance` on grammar values, which is
  what a glossary row links to. Worth fetching before first render:
  68 KB, and without it every code links nowhere.
- **After `roots.json`.** Root meanings, the lexical headword, and
  `define`, which reads English backwards into lexical cores.

`info()` reports what has landed, and `apiVersion` alongside it. Check
that against the version your page was written for: a mismatch means a
stale cached bundle, and saying so beats rendering a shape you misread.

## The calls

Argument and return types are in `ithkuil.d.ts`. What is worth saying
beyond them:

- **`parse(text)`** returns one entry per written word. A concatenation
  chain answers in `members`, one per formative in written order, and
  leaves `segments` empty: a chain has no single breakdown, and
  flattening one loses which member each slot belongs to. A word that
  will not read stays in place carrying `error` rather than failing the
  span. ASCII digraphs are accepted (`t,` for `ţ`, `sq` for `š`), so no
  keyboard layout is needed. `violations` is separate from `error`: a
  word can parse and still be unpronounceable, so a clean parse with a
  §2 violation is a real state and not a contradiction.
- **`compose(expr)`** builds a word from a gloss expression. `-`
  separates slots, `.` joins category values inside one, `/` binds a
  degree or a case to a head, `_` trails the affix Type, and `{Ca}` is
  an all-default Ca that still marks that boundary. Affixes written
  before the Ca land in Slot V. `ml`, `S2.CPT-ml-ERG`,
  `m-SYS/5_2-{Ca}-DCD/1_2`. The `gloss` that comes back is the canonical
  gloss of what was built, which is not always the expression asked for:
  rendering is canonical, so an equivalent spelling returns normalized
  and the page can show that it did.
- **`positions()`** is the word builder's frame: which grammatical
  category is edited in which slot. Populate a control's options with
  `table(category)` using the names it gives. Do not hard-code the
  mapping; it is checked against the grammar on our side and would be a
  second, unchecked copy on yours.
- **`note(abbrev)`** is what a gloss code links to. Most values have no
  note, which is normal: only the ones with something surprising about
  them carry one, and "no note for this one" is the honest thing to show
  rather than something invented to fill the panel.
- **`input(ascii)`** runs the digraph input method over a field's whole
  contents and is stateless. Hold the ASCII the reader typed, ask what
  it looks like, and render `pending` dim: `input("mat")` gives
  `committed: "ma"`, `pending: "t"`, because a following `,` would still
  turn that `t` into `ţ`.

## Rendering a gloss

`parse` returns the gloss twice: `gloss` as one line, and `glossTokens`
as its pieces. Concatenating every token's `text` reproduces `gloss`
byte for byte, so rendering the pieces shows exactly what the glosser
wrote.

```js
word.glossTokens.map(t =>
  t.kind === "code" ? link(t.text) : escape(t.text)).join("")
```

Both are sent on purpose. Joining is trivial and the gloss syntax is
not: which mark separates two slots and which binds a degree to its
affix stays on this side. Do not split the string yourself. The comma is
the trap, `nt,l` is one cluster in the ASCII notation, not two atoms
around a separator.

`kind` says how a piece is written, not what it resolves to. A `code` is
worth offering `note()` for; whether one exists is `note()`'s answer.

## What is not there yet

Stated so a page is not built expecting it:

- Referentials, combination referentials, affixual adjuncts and carrier
  adjuncts get a type and a gloss and no `segments`. That is not missing
  wiring: they have no slot structure to break down, and the CLI shows
  none for them either. Formatives, modular adjuncts and concatenation
  chains all have one, and a word that fails on a single slot still
  comes back with its shape split.
- `compare` fails outright on those same classes, with
  `"Affix has no slot breakdown to compare"`.
- `search` takes a query and nothing else. The category, exact, form and
  limit filters the CLI has are not exposed.
- There is no way to list all 528 affixes or page the roots. Browsing
  the lexicon needs a query to start from.
