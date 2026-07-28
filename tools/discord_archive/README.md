# Discord archive

Mirrors the community Discord and turns it into a corpus of attested
Ithkuil, which is what `fullparse/corpus_regression_test.go` is written
against. Attested usage catches defects that constructed examples do
not, because nobody writing a test case thinks to put a §3.1.7 chain
next to a Slot IV/VI shortcut.

Everything here is generated data, so it lives outside the source tree,
under `$XDG_DATA_HOME/ithkuil/discord/` (`~/.local/share/ithkuil/discord/`
when `XDG_DATA_HOME` is unset). Set `ITHKUIL_DISCORD_DIR` to move it.
See `paths.py`.

## Getting a token

The Discord REST API wants a *user* token, not a bot token. Discord
deletes `localStorage` off the main window, but an iframe on the same
origin still has it, so paste this into the browser console with
Discord open:

```js
(() => {
  const f = document.createElement('iframe');
  document.body.append(f);
  const t = JSON.parse(f.contentWindow.localStorage.token);
  f.remove();
  copy(t);
  console.log(t);
})()
```

`copy()` is a DevTools helper, so the token lands on the clipboard as
well as printing. Both Chrome and Firefox refuse pasted console input
until you type `allow pasting` once. Discord also prints a large red
self-XSS warning, aimed at people being talked into pasting something
they do not understand.

This is version-independent. The `webpackChunkdiscord_app` snippets
found elsewhere break whenever Discord reshuffles its bundle.

Tokens rotate on password change and on logout, so treat them as
short-lived. `mirror.py` writes `_progress.json` per channel and
resumes from it, so a run that dies on an expired token costs nothing
but a restart.

Automating a user account is against Discord's terms of service. The
rate limiting below is what keeps a run unremarkable.

## Giving the token to the tools

Either `DISCORD_TOKEN` in the environment, or a file at
`$XDG_DATA_HOME/ithkuil/discord/token`. The file is the usual route,
since the environment does not survive between commands:

```bash
echo -n 'YOUR_TOKEN' > ~/.local/share/ithkuil/discord/token
```

Never put it in the repo. There is no `.gitignore`, by design.

## The pipeline

Run from this directory, inside `nix develop`.

```bash
python3 probe.py [GUILD_ID]      # check access, list channels
python3 mirror.py [GUILD_ID]     # fetch raw message JSON
python3 extract_v4.py            # pull out Ithkuil text and translations
python3 words.py                 # reduce that to candidate word tokens
python3 analyze.py               # idioms, frequencies, technique discussions
```

`GUILD_ID` defaults to the original server. `paths.GUILDS` names the
servers we mirror. The community moved to "New Ithkuil Study Group
(hlacnyo'unfé-ediláu)" (`1345994901200044072`) in 2025; the original
(`131937038139260928`) is kept because it holds everything before that.
The new server keeps its language channels under a category named
MAŢŘËULLAIT, which is the endonym this project's README describes.

`probe.py` is read-only and fails in one API call, so run it first
rather than discovering a bad token part-way through an archive.

`mirror.py` walks channels newest-first, honours 429 `retry_after`,
sleeps 0.5s between batches, and stops at `DISCORD_MAX_BYTES` (10GB
default). It archives threads too. Channel order comes from
`PRIORITY_CHANNELS` where that list applies, so a run cut short by the
disk budget still gets the v4 material; a server not in the list is
walked in API order, which is all we can do without knowing it.

Then, from the repo root:

```bash
go run ./tools/discord_archive/fidelity ~/.local/share/ithkuil/discord/extracted/v4_words.txt
```

`fidelity` is the audit, and it asks two separate questions.

Coverage: what fraction of the corpus do we understand at all? That
goes through `tokenize.ClassifyWord`, the same entry point the CLI and
the MCP server use, so a referential or a bias adjunct counts as
understood. Asking `fullparse.Formative` directly instead, as this
tool used to, scored every non-formative word as a parse failure: 372
of them, over half of an apparent 696-word gap. A referential is not a
broken formative.

Fidelity: of the words that are formatives, is the round trip lossless
(parse, render, parse again must land on the same gloss), is what we
emit a legal word at all, and does our canonical spelling match what a
human wrote? The first two are defects. The last is a style
difference, because the renderer canonicalizes and the grammar permits
several spellings of one word.

Only formatives get the fidelity checks, because they are the only
token kind with a renderer.

What stays unclassified is printed as a triage list, grouped by the
formative parse error that best describes the shape. Those errors are
a diagnostic, not a claim that the word was meant to be a formative;
the formative decoder simply gets furthest into a word of any of the
classifiers.

`words.py` has unit tests, because a tokenizer bug reads as an
implementation bug: junk tokens land in the audit as parse failures
and make the parser look worse than it is.

```bash
python3 -m unittest discover tools/discord_archive
```

## What counts as attested usage

`words.py` decides what reaches the corpus.

A token is a whole run of letters, taken whole and then required to
lie inside the V4 alphabet. Matching the alphabet directly instead
would cut a word around any foreign letter and feed the fragment to
the audit: `ıţkuil` became `ţkuil`, which is not a word anyone wrote.
Dropping the chunk removed 260 of them and moved the parse rate from
77.8% to 80.8% without touching the parser.

Channels, per server. The original ran every version of the language
side by side, so only `#v4-only` and `#works-v4` qualify. The study
group was founded after v4 won, so all of it does.

Bots are excluded. Their output is another implementation's opinion
rather than attested usage, and the dictionary bots post bare roots,
which are not words and cannot parse as formatives. `IthGlyph` alone
accounts for most of `#spam-bot-talk`.

## Why the corpus starts at 2023

`words.py` drops messages from before 2023-01-01. The morphology we
implement is v1.3.1, published that year, and the channels predate it
by three years. Older messages are written in earlier drafts, sometimes
in an alphabet v4 no longer has (dotless `ı`, grave `ì` and `ù`).

Measured against our parser the break is sharp. Every half-year through
2022 parses between 36% and 56%; every half-year from 2023 on parses
between 61% and 80%. Scoring the parser against a grammar it does not
implement measured nothing, and it hid real defects behind noise.

A word from that era can still earn a place in the regression tests,
but only when the rule it breaks is a v1.3.1 rule.
