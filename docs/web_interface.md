# Web Interface Design

A design for a website over this repo. It says what the site does and why
it is shaped that way. It does not specify markup, styling, or a build
pipeline, and it settles no question that the code has not yet raised.

## What the site is

One page that reads and writes Ithkuil. You put a word or a text in, and
it tells you what the word is made of and what it means; you change the
pieces, and it tells you what the word becomes. Both directions, one
place, on the same word.

That last clause is the whole design. Everything below follows from it.

Not in scope: machine translation into English (the `ithkuil-translate`
skill covers that, and it needs a model we are not going to ship),
the writing system (it lives on the `writing` branch, bound for its own
repository), earlier versions of the language, and accounts of any kind.

## What the two existing attempts teach

**Ithkapp** (github.com/chromonym/ithkapp, Vue 3, about 5,700 lines) is a
word builder. A grid of "option boxes", one per grammatical category,
each a dropdown or a text field; pick values and a word assembles itself
in a footer that stays pinned, showing the romanization, the IPA, and a
gloss. Which boxes appear depends on a word-type menu (formative,
referential, five kinds of adjunct); which are enabled depends on other
boxes. Clicking a box's title opens a modal explaining the category. A
sidebar holds a sentence as an ordered list of built words, each with a
free-text description, draggable, savable to JSON.

What it gets right, and what we should keep: the pinned live result, so
you always see what you are making; per-category explanation one click
from the control it explains; progressive disclosure by word type, which
is the only thing that makes thirty-odd controls survivable; and treating
a sentence as a first-class object rather than a word at a time.

What it gets wrong, and its author says as much in the README: the
grammar is re-encoded in the front end. `grammardata.json` is a parallel
authority to Quijada's tables, and the dependencies between categories
live in the template as `:disabled` expressions on the components, so
the rule "Similarity is inapplicable under a uniplex configuration" is a
string in an attribute. It also goes one way only. It builds words; it
cannot read one. Paste a word you found and it has nothing to say.

**The gloss-to-prompt tool** in the translator project (vanilla
TypeScript over Vite, about 1,600 lines) goes the other way and stops
early. Paste a morphological gloss, and it expands each token into a
definition drawn from curated tables, wraps the result in instructions,
and hands you a prompt to paste into a model elsewhere. Around it: an
example navigator over the published corpus with previous, next, random
and a direct index; a reference panel showing Quijada's own English for
the current example; a gloss guide in a dialog; a methodology section
saying where every number came from; and an attribution footer.

What it gets right: it is honest about provenance in the interface
itself, not in a README. It also picked the corpus as the browsing
surface, which is correct, examples are how people actually learn this.

What it gets wrong: it starts from a gloss. Getting the gloss is the hard
part, and it is exactly the part we have solved. It is a clipboard tool
wearing a language tool's clothes.

Between them they bracket what we should build. Ithkapp writes and cannot
read. The prompt tool reads a gloss it cannot produce. We can do both,
from the same state, because `slots.ToGrammar` and `slots.FromGrammar`
are one table read in two directions and `roman` keeps reading and
writing in one package for the same reason.

## The one structural decision

The grammar lives in the Go and the browser does not get a copy.

Both prior attempts re-encode grammatical knowledge in JavaScript because
they had nothing else to call. We do: 31 consonants and 9 vowels in
`phonology`, the Ca tables in `allomorph`, every lookup table in `parse`,
the canonical-form choices in `slots.FromGrammar`, and a test suite that
sweeps one minimal word per grammatical value through both arms. Copying
any of that into TypeScript is how the site starts lying, and it will lie
first in the places nobody checks.

So the front end holds no domain logic. It holds a text field, a
selection, and whatever the Go last returned. That is a small enough
state to need no framework, and it is why this document does not name
one: the front end has nothing to organize.

It reaches the browser as WebAssembly, so the site is static: no server
to run, no rate limit, no privacy surface, offline once cached, and it
deploys anywhere a file can be put. `cmd/ithkuil-wasm` is that module and
`tools/build_wasm.sh` builds it.

Size, measured rather than guessed, brotli'd, which is the only figure
that matters over the wire. The full numbers and the rejected cuts are
in the head of the build script; the shape of it is that the standard
toolchain emits 1.01 MB and TinyGo with `-no-debug` and `wasm-opt -Oz`
emits 0.31 MB for the same code, a third of it. TinyGo compiled this
module without complaint, `reflect` and `encoding/json` included, so the
usual reason to stay on the standard toolchain does not arise. Add
0.32 MB for a brotli'd `data.json` and the page costs about 0.63 MB
once, behind an immutable URL.

Two consequences worth stating, because they constrain the code and not
just the build:

- `lexicon` must not import `store`. The SQLite driver has no `js/wasm`
  build, and a page has no file to open, so the store direction lives in
  `store.LoadLexicon` and the browser hands `lexicon.Parse` the bytes it
  fetched.
- Loading the lexicon is a separate call from starting the module, and
  it merges rather than replaces. The parser needs no lexicon at all,
  only meanings do, so the wire is three files: the module at 322 KB,
  affixes at 54 KB, roots at 260 KB. A word can be read and its slots
  shown before either lexicon file lands, and the affix ladder works as
  soon as the small one does. Compressing harder is not the lever;
  minifying the JSON saves 1 KB and dropping the Wikidata Q-IDs saves 2,
  because Brotli already eats that redundancy.

## The contract

The boundary is a written contract, not whatever the internal types
happen to serialize as. `code/api` declares it in Go with explicit json
tags; `web/ithkuil.d.ts` declares the same shapes in TypeScript; and
`api/dts_test.go` fails when the two disagree on a name, a field, or
whether a field is optional.

That test is the point. A hand-written declaration file is a lie waiting
to happen: nothing in a Go build reads it, so renaming a field leaves
TypeScript compiling happily against a shape the module stopped
answering with. Before `api` existed the page would have read `Differs`
and `Chunk` and `Encodes`, Go field names in Go capitalization, chosen
for Go callers and changing whenever a Go caller wanted them to.

Three conventions, each a decision rather than a default:

- Wire keys are camelCase, and a type with an untagged field fails the
  test rather than sending the Go name.
- Every call answers in one envelope, `{ok: ...}` or
  `{error: {message}}`, so TypeScript has a union to discriminate and
  the front end unwraps in one place. Nothing throws: in this domain a
  rejected word is an ordinary answer, and the reason belongs beside the
  word that produced it rather than in a catch block.
- `Info.apiVersion` is bumped when a shape changes in a way an older
  page would misread, so a stale cached bundle can say so instead of
  rendering nonsense.

`docs/web_api.md` is how to use it from a page. Everything a type
declaration can state lives in `ithkuil.d.ts` instead, so the two do not
overlap and only one of them can go stale.

`api` builds on every platform, which is what lets the normal suite
cover it while only the thin `cmd/ithkuil-wasm` adapter is js/wasm-only.
If the HTTP server below ever gets built, it serves these same shapes
rather than inventing a second contract.

The rejected alternative was a small Go HTTP server exposing the same
calls the CLI makes. It has the advantage that the code is exactly what
the CLI runs, with no build constraints and the lexicon left in SQLite.
It costs something to run and keep running, and a round trip on every
keystroke in a UI that re-renders as you type. At 0.63 MB once, the
static build wins on both counts.

## The text is the state

One canonical piece of state: the text in the box. The parse, the
glossary, the option boxes, the URL, all of it derives from that string.

This is the substantive departure from Ithkapp, which holds a tree of
selected options and derives the word from it. Holding the text instead
buys three things at once. Sharing works, because the URL is just
`?t=<text>`, and Ithkapp's "Create Hyperlink" button raises a
"not available" alert precisely because its state is a tree. Reading
works, because the state is what a reader already has. And the composer
cannot drift from the parser, because the composer's output is fed back
through the parser to produce what you see.

`render` is canonical, one grammar to one surface, so the round trip has
a fixed point: type a word, change nothing, get the same word back, or
get told your spelling was non-canonical and see which. That is a
feature, and Ithkapp cannot offer it.

A text, not a word. `roman.ParseText` and `roman.Tokenize` already work
per text, and `Tokenize` pairs each romanization with either the word it
produced or the reason there is none. So the unit is a sentence, the
words in it are a row of tabs or chips, and one of them is selected. The
sidebar sentence-builder disappears into the text field, which is where
sentences already were.

## The views

Four views on the selected word, sharing that one text. `ithkuil-wasm`
already exposes what each of them needs: `parse`, `compose`, `compare`,
`search`, `define`, `categories`, `table`, `affix`, `fromASCII`. Layout,
top to bottom, on a phone; the same regions side by side on a wide
screen:

    the text field
    the word chips, one per word, the failed ones marked
    the selected word, large, with its gloss under it
    the view, one of the four below

**Read** is the default and needs no new code. `view.Segments` gives the
phonetic-segment table (segment, slot, what it encodes), `view.Headword`
gives the root's lexical entry for its stem and specification, and
`view.Glossary` gives the category / code / name / meaning rows. The CLI
already prints exactly this; only the table drawing is CLI-specific.

**Build** is Ithkapp's contribution, rebuilt on the parse. The controls
are one per slot rather than one per category, following the slot
structure the language actually has, and their options come from the
grammar tables rather than from a list in the page. Changing one produces
a new `Formative`, which renders to a new word, which replaces the text,
which re-parses. The loop closes through the Go every time, so an
impossible combination fails where it should, in the code that knows why,
and reports the rule and not a guess.

Two of Ithkapp's ideas survive intact here and should be copied
deliberately. Progressive disclosure by word class: a bias adjunct has
one control and a formative has thirty, and showing thirty greyed-out
controls to someone writing a bias adjunct is worse than showing none.
And explanation adjacent to the control, which is the next section.

**Compare** is `view.PairSides`, `view.SlotDiff` and `view.GlossDiff`,
already written and already the most instructive thing the CLI does. Two
words, slot by slot, with the differing rows marked. It is how you see
that "Maţřëullait" and "Malëuţřait" are the same morphemes with SYS in a
different slot. Nothing else in the ecosystem does this, and it is nearly
free.

**Learn** is three bodies of material and one query box over all of
them. `search.SearchGrammar` and `dictionary.Index` answer "what does ATV
mean" and "what is the root for *speak*", grammar hits first, as the CLI
does. But a search box is only half of it, because you cannot search for
what you do not know the name of, so each body is also browsable whole:

- The grammar, 294 values in 23 categories. `search.Categories` lists
  them and `search.Filter` gives one category's rows, which is the same
  call the builder's controls are populated from. A category read
  top to bottom is a lesson; the same rows in a dropdown are a control.
- The lexicon, 5,891 roots, each with up to four stems and the
  specification variants. The entry point is the English index, because
  that is the direction a learner arrives from.
- The affixes, 528 of them across nine degrees each. This is the part
  neither prior tool made learnable and the part learners complain about
  most. The unit is one affix's whole degree ladder, read down: degrees
  are a gradient, and a table of nine rows shows the gradient in a way
  nine separate lookups never do.

Every row in all three is a link into Build, because the fastest way to
understand a value is to see a word that carries it and one that does
not, which is Compare with the baseline from `inventory`.

## Explanation, one click from the code

Every code the glossary prints is a link. Clicking it shows the entry
from the grammar table if the value has one, and the
relevant passage of `docs/reference/morphology.md` otherwise.

This is Ithkapp's modal, with a better source behind it. Ithkapp wrote
its own descriptions and mixed them with material from two versions of
the language; ours are `explanation` and `guidance` on the grammar
value itself, authored rather than transcribed, saying how a category
lands in English, with the transcription winning wherever the two
disagree. They are data and not a document precisely because this is
what wants them: a browser cannot open a markdown file, and a note that
belongs to a value should travel with the value, so the same text
serves a glossary row, a builder control and a reference table without
three lookups. 160 of the 294 values have one, and 42 further topics
cover what belongs to no single value. The rest are absent because they
hold no surprise, and the interface should say "no note for this one"
rather than manufacture something to fill the panel.

## Typing the letters

`phonology.InputState` with `FromASCII` and `ToASCII` already turns ASCII
digraphs into the orthography, `aa` to `ä`, `t,` to `ţ`, `sq` to `š`,
`dz` to `ẓ`. It backs `cmd/ithkuil-input` and it belongs in the text
field on the first day. Nobody has these characters on a keyboard, and a
tool that demands them before it will speak is a tool nobody uses twice.
Pending characters show dim, as they do in the TUI.

## Failing in public

Words that do not parse are the interesting case, not the error case. A
chip for a failed word stays in the row, marked, and selecting it shows
why: for a phonotactic failure, the section 2 rule it breaks; for a
parse failure, the reason `Tokenize` gave.

No guesses, no "did you mean". The parser knows what it saw and what it
expected; that is the whole of what the interface should say. A word that
fails on our side and not in the language is a defect in this code, and
the fastest way to get one reported is to show what we thought was wrong
with it.

## Corpus and inventory as content

Two bodies of test data are also the site's best browsing material.
`corpus` holds 384 published example sentences with Quijada's own English,
which is what the prompt tool navigated and was right to. `inventory`
holds one minimal word per grammatical value, each differing from a fixed
baseline in that value alone, which is a guided tour of the grammar that
already exists and is already checked. "Show me a word that differs only
in essence" is a link, and the answer is a Compare view.

## Attribution

Both prior sites carry it and so must this one. The language, the grammar
and the lexicon are John Quijada's, used under the terms published on
ithkuil.net; the site is an unofficial derivative. The lexicon data comes
from the community spreadsheet. Publishing also forces a question this
repo has not answered: there is no LICENSE file here, and the merged
material arrived from a GPL-3.0-only project. That has to be settled
before anything is deployed, not after.

## What is missing

Two gaps, both real, neither blocking:

- No IPA. Ithkapp shows a pronunciation under every word and we cannot,
  because nothing in `phonology` maps the orthography to IPA. The
  inventory tables are the natural home for it if we want it.
- No audio, and no plan for it.
