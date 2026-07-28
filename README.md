# Ithkuil

**Ithkuil** is a constructed language created by John Quijada, designed to
express deeper levels of human cognition with precision and concision. Where a
natural language leaves most of what a speaker means to context, Ithkuil makes
it grammar: every word states its own aspect, its speaker's certainty, how the
referent is bounded, and much else besides. The morphology is highly regular,
which is what makes the language tractable to software at all.

| Version | Year | Name | Description |
|---------|------|------|-------------|
| I | 2004 | Iţkuîl | Original grammar with consonant grades and biliteral roots |
| II | 2007 | Ilákš | Revised with tonal vowel system |
| III | 2011 | Iţkuil | Major redesign with Designation, Pattern, and Sanction |
| **IV** | **2023** | **Iţkuil** | **Current version, implemented here (morphology v1.3.1)** |

Quijada calls all four versions *Iţkuil*, disambiguating v3 and v4 in running
text as *êpal Iţkuil* and *wiosaḑca Iţkuil*. The community nicknames *Elartkʰa*
(v3) and *Maţřëullait* (v4) are not official names, and Quijada has said so
explicitly; they are in wide use anyway, and *Maţřëullait* is this project's
canonical test word.

*Maţřëullait* replaced an earlier form, *Malëuţřait*, in late 2024. The two
words are the same morphemes: the SYS affix moved from Slot VII to Slot V, so
it applies to the stem alone instead of having scope over the Ca complex, and
the Ca geminates (`-l-` → `-ll-`) to mark that Slot V is filled.

## What This Repo Is

A set of tools for working in Ithkuil, written in Go, covering three jobs.

**Phonology**: typing the orthography from an ASCII keyboard, and checking that
a word is pronounceable Ithkuil at all, that its clusters and vowel sequences
and stress obey the phonotactics.

**Grammar**: taking a word apart into its slots and saying what each one
encodes, building a word back up from a specification of what it should mean,
and looking up the grammatical inventory and the lexicon behind both.

**Agent translation**: the same parser, composer, and lexicon exposed over the
Model Context Protocol, so an AI assistant translating into or out of Ithkuil
works against the real grammar instead of guessing at it. The `ithkuil-mcp`
binary is that server; see [Building and Running](#building-and-running).

Everything below is one of the CLI's subcommands. `ithkuil <sub> --help` prints
the flags for any of them.

## Typing Phonetically

The orthography uses diacritics that aren't on a keyboard, so every character
that carries one also has a two-keystroke ASCII spelling. The notation is a
pure recoding of the Unicode text, one character to one digraph, and reversible
in both directions. Anything with no digraph, `'` (glottal stop) included,
passes through untouched, so an already-Unicode word is left as it is.

| | Char | ASCII | Char | ASCII | Char | ASCII | Char | ASCII | Char | ASCII |
|----------|------|-------|------|-------|------|-------|------|-------|------|-------|
| Umlaut   | ä | `aa` | ë | `ee` | ö | `oo` | ü | `uu` | | |
| Cedilla  | ţ | `t,` | ḑ | `d,` | ļ | `l,` | ç | `c,` | | |
| Háček    | š | `sq` | ž | `zq` | č | `cq` | ň | `nq` | ř | `rq` |
| Underdot | ẓ | `dz` | | | | | | | | |
| Stress   | á | `a/` | é | `e/` | í | `i/` | ó | `o/` | ú | `u/` |
| Stress   | â | `aa/` | ê | `ee/` | ô | `oo/` | û | `uu/` | | |

A trailing `/` stresses the vowel you just typed, umlaut included:
`hala/` → *halá*, `malee/ut,rqait` → *malêuţřait*. A `/` that follows anything
else is left as a literal `/`, so type it right after its vowel.

The vowel `i` has no umlaut and so no doubling rule: `ii` stays `ii`. The other
four group to the right when repeated, `eee` → *eë* and `eeee` → *ëë*, which is
how you write an umlaut adjacent to its own plain vowel.

`ithkuil-input` converts a whole line at a time. It runs as a raw-mode TUI that
shows pending keystrokes dimmed until they resolve into a digraph or are broken
by a different character, and as a pipe filter when stdin isn't a terminal:

```bash
ithkuil-input                           # interactive
echo 'Mat,rqeeullait' | ithkuil-input   # batch: ASCII in, Unicode out
```

The other subcommands don't need it. Every word argument to `ithkuil` is read
through the notation, as are the root and affix clusters inside a `compose`
expression, and it is what glosses print clusters in, so gloss output feeds
straight back in:

```bash
ithkuil parse mat,rqeeullait            # same word as Maţřëullait
```

## Analyzing Words

`parse` takes Ithkuil text apart. It pairs a phonetic segmentation of each word
with a glossary that expands every code it used, so nothing in the breakdown
has to be looked up elsewhere:

```
$ ithkuil parse Maţřëullait
maţřëullait
  PHONETIC  SLOT  ENCODES
   ∅        Vv    S1 / PRC
   m-       Cr    Root "m"
  -a-       Vr    STA / BSC / EXS
  -ţř-      Cs₅₁  SYS
  -ëu-      Vx₅₁  DEG5
  -ll-      Ca    UPX / CSL / M / DEL / NRM
  -ai-      Vx₁   DEG1
  -t        Cs₁   DCD
   ∅        Vc    THM
  ROOT
  "m" / S1 / BSC — stem 1: linguistic utterance for communication
  CATEGORY       CODE   NAME                    MEANING
  version        PRC    Processual              the act/state as a process
  function       STA    Static                  entity as a state, condition, or quality
  context        EXS    Existential             pure existence (default ontology)
  affix          SYS/5  Networks & Systems      A feedback-driven/self-sustaining/autopoietic system based on X
  configuration  UPX    Uniplex                 a single instance
  affiliation    CSL    Consolidative           no specific affiliation among members
  perspective    M      Monadic                 single instance/individual
  extension      DEL    Delimitive              default — bounded, demarcated
  essence        NRM    Normal                  the entity as it actually exists
  affix          DCD/1  Deictic Demonstratives  this
  case           THM    Thematic                inactive participant (CONTENT role)
```

Words are classified before they are parsed, so a sentence can mix formatives
with referentials and the various adjuncts, and each is broken down as its own
kind of word. `--short` collapses each one to a single line of surface, type,
and gloss, which is the form to reach for on a whole sentence:

```bash
ithkuil parse --short 'Maţřëullait wimlo'
```

Phonotactics are checked first, and a word that isn't pronounceable Ithkuil is
reported with the rule it breaks instead of being forced into a slot breakdown:

```
$ ithkuil parse tttest
tttest  1.7: triple consonant (cluster ttt)
tttest  6.2: not permissible word-initially (cluster ttt)
```

That check is why there is no separate validation command; parsing a word
validates it. The exit status is 1 if any word failed.

## Comparing Two Words

`compare` answers what one letter is doing, by laying two breakdowns side by
side and marking only the slots that disagree:

```
$ ithkuil compare marçat marcat
   SLOT  marçat                           marcat
   Vv     ∅    S1 / PRC                    ∅    S1 / PRC
   Cr     m-   Root "m"                    m-   Root "m"
   Vr    -a-   STA / BSC / EXS            -a-   STA / BSC / EXS
≠  Ca    -rç-  MDF / COA / M / DEL / NRM  -rc-  DSS / COA / M / DEL / NRM
   Vx₁   -a-   DEG1                       -a-   DEG1
   Cs₁   -t    DCD                        -t    DCD
   Vc     ∅    THM                         ∅    THM

   DIFFERENCES
   CATEGORY       marçat                           marcat
   configuration  MDF  Multiplex Dissimilar Fused  DSS  Duplex Similar Separate
```

A word that won't decode is still comparable: the shape split outlives the
grammatical read, so a word can be held up against the same word with one
change that breaks it, and the marked rows show how the change re-split
everything after it. With one such word in the pair, shape alone decides what
is marked, and the differences table is dropped.

For a concatenation chain the members pair off from the parent end, since the
parent comes last and is what a standalone word is the counterpart of. Each
pair gets its own table, and a longer chain's leading dependents are reported
as unpaired rather than dropped.

## Building Words

`compose` runs the other direction: give it what the word should mean and it
prints the word. The expression is the same gloss syntax `parse` prints, so the
two are inverses. Slots are separated by `-`, sub-fields by `/`, and the Ca
complex by `.`:

```
$ ithkuil compose ml
mlala
-ml-'yellow'

$ ithkuil compose S2/CPT-ml-ERG
wimlo
S2/CPT--ml-'gold (color)'-ERG

$ ithkuil compose 'S2/CPT-ml-DYN/OBJ-MSS.G-DEV/3-ERG'
imlötrebo
S2/CPT--ml-'gold (color)'-DYN/OBJ/EXS-MSS.G-DEV/3-ERG
```

The root is a consonant cluster, `(ABBREV)/degree` for an affix used as a root,
or `(1m+2p)` for a referential root. An affix is written `Cs/degree` or
`ABBREV/degree`, with an optional `_2` or `_3` type tag. Position carries
meaning: an affix written before the Ca applies to the stem alone, one written
after it has scope over the Ca. Write `{Ca}` for an all-default Ca that still
has to mark that boundary, as *maţřëullait* does:

```bash
ithkuil compose 'm-SYS/5_2-{Ca}-DCD/1_2'   # → maţřëullait
```

Every slot you leave out takes its default, and the surface printed is
canonical: one Formative has exactly one spelling here, even where the grammar
would permit several. See [SPEC.md](SPEC.md) for what canonical means and which
optional shortenings it decides between.

## Grammar Lookup

`search` looks a term up in the grammar inventory and in the lexicon at once,
grammar hits first, since a three-letter query is far more often a grammatical
abbreviation than a root:

```bash
ithkuil search ERG               # the Ergative case, then roots and affixes matching
ithkuil search --category Case   # every entry in one category
ithkuil search --exact THM       # exact abbreviation only
ithkuil search --form ëu         # what a surface vowel or cluster can encode
```

With no arguments it lists the categories available to `--category`. A section
with no hits isn't printed, and `--form` skips the lexicon half, since asking
what a vowel encodes is a question only the grammar answers.

`define` reads the lexicon backwards, from English to Ithkuil. It answers with
lexical cores, a root plus the stem and version and specification that select
the sense, not with whole words, because case and illocution belong to the
sentence rather than to a dictionary entry:

```
$ ithkuil define crisis
crisis
  ojḑal            S0/PRC-jd,           predicament/crisis/dilemma
```

Coverage of English is partial by nature: the index says what the lexicon
already happens to name in English, and is not a dictionary of English.

## Building and Running

Requires Go 1.25+. The repo uses [Nix](https://nixos.org/) to pin the
toolchain, but `go` from any source works. The Go module is `code/`.

```bash
nix develop                     # dev shell with go and python3 on PATH
python3 tools/build_db.py       # build the data store the CLI reads
cd code
go install ./cmd/...            # ithkuil, ithkuil-mcp, ithkuil-input → $GOBIN
```

Pass `--data FILE` to read a data store other than the default
`$XDG_DATA_HOME/ithkuil/data.db`, and `--color=auto|always|never` to control
ANSI styling.

`ithkuil-mcp` speaks the Model Context Protocol over stdio, exposing the same
parser, composer, and lookups as tools. Point an MCP client at the installed
binary; it takes the same `--data` flag.

### Gloss Syntax

The gloss is both the output of `analyze` and the input to `compose`. Every mark in it has exactly one job, so what a token is follows from its shape:

| Mark | Job | Example |
|------|-----|---------|
| `-` | separates slots | `S2.CPT-ml-ERG` |
| `.` | joins category values in one slot | `DYN.OBJ.FNC`, `MSS.G`, `ASR.RPR` |
| `/` | binds a degree or a case to a head | `DEV/3`, `ACC/INS`, `(1m)/AFF` |
| `_` | trails the affix Type | `t/1_2`, `IAC/PRP_3` |
| `:` | tags a structured body | `Ca:MSS.G`, `NOM:1m` |
| `()` | a head built from referents or a Cs | `(1m+2p/BEN)`, `(CTR)/1` |
| `+` | joins referents | `1m+2p` |
| `{}` | structural, not a morpheme | `{Ca}`, `{parent}` |

The whole gloss is ASCII, including the root, which uses the digraph notation from the table above. It has to be typable on an ordinary keyboard, since it is an authoring syntax and not only an output format.

```bash
ithkuil compose 'ml-Ca:PRX-ERG'         # a Ca stacked on the Slot VI Ca
ithkuil compose 'ml-ACC/INS-ERG'        # a §3.9.2 case-accessor
ithkuil compose 'ml-(1m)/AFF-ERG'       # a §4.6.5 Column-4 referential
```

## References

- **Morphology v1.3.1** (2023-02-11): the primary reference document
- **Official website**: [ithkuil.net](http://ithkuil.net)
- **Community archive**: [ithkuil.place](https://ithkuil.place)

The transcribed grammar lives in [docs/reference/](docs/reference/), and
[SPEC.md](SPEC.md) describes the formats this project converts between.

## Acknowledgments

Ithkuil is written by **John Quijada**, who has been developing the language since the 1970s.

Thanks to **ngoriyev** for the Kotlin codebase [IthkuilGloss](https://github.com/ngoriyev/IthkuilGloss), which was used as a reference for some parts of this code.
