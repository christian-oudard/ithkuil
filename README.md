# Ithkuil V4 Grammar Implementation

A Go implementation of the grammar of **New Ithkuil** (v1.3.1, 2023), the constructed language created by John Quijada. Ithkuil is a philosophical language designed to express deeper levels of human cognition with precision and conciseness. It features a highly regular morpho-phonological system.

This project implements a parser, renderer, and glosser for Ithkuil formatives and adjuncts. In New Ithkuil, the language's name for itself is *Malëuţřait*. The goal of this codebase is to serve as a canonical programmatic reference for the language's grammar.

## Version History

| Version | Year | Native Name | Description |
|---------|------|-------------|-------------|
| I | 2004 | Iţkuîl | Original grammar with consonant grades and biliteral roots |
| II | 2007 | Ilákš | Revised with tonal vowel system |
| III | 2011 | Elartkha | Major redesign with Designation, Pattern, and Sanction |
| **IV** | **2023** | **Malëuţřait** | **Current version - implemented here (morphology v1.3.1)** |


## What This Project Does

Given an Ithkuil word or sentence, the parser decomposes it into its morphological slots, looks up roots and affixes in the lexicon, and renders a learner-oriented breakdown:

```
$ ithkuil analyze Malëuţřait
malëuţřait
  PHONETIC  SLOT  ENCODES
   ∅        Vv    S1 / PRC
   m-       Cr    Root "m"
  -a-       Vr    STA / BSC / EXS
  -l-       Ca    UNI / CSL / M / DEL / NRM
  -ëu-      Vx₁   DEG5
  -ţř-      Cs₁   SYS
  -ai-      Vx₂   DEG1
  -t        Cs₂   DCD
   ∅        Vc    THM
  ROOT
  "m" / S1 / BSC — stem 1: linguistic utterance for communication
  CATEGORY       CODE   NAME                    MEANING
  version        PRC    Processual              the act/state as a process
  function       STA    Static                  entity as a state, condition, or quality
  context        EXS    Existential             pure existence (default ontology)
  configuration  UNI    Uniplex                 a single instance
  affiliation    CSL    Consolidative           no specific affiliation among members
  perspective    M      Monadic                 single instance/individual
  extension      DEL    Delimitive              default — bounded, demarcated
  essence        NRM    Normal                  the entity as it actually exists
  affix          SYS/5  Networks & Systems      A feedback-driven/self-sustaining/autopoietic system based on X
  affix          DCD/1  Deictic Demonstratives  this
  case           THM    Thematic                inactive participant (CONTENT role)
```

This tool provides:
- **Morphological parsing**: Decomposition of formatives into their 10 slot structure
- **Lexicon lookup**: 4,720 roots and 527 affixes from the official lexicon
- **Word type classification**: Formatives, referentials, bias adjuncts, register adjuncts, modular adjuncts, affixual adjuncts, carrier adjuncts, combination referentials
- **Composition**: Building formatives from grammatical specifications
- **Grammar search**: Looking up grammatical categories, roots, and affixes by keyword
- **Sentence glossing**: Multi-word sentence parsing with concatenation chain detection
- **MCP server**: Same parser/composer/lexicon exposed as Model Context Protocol tools

## Implementation Status

### Fully Implemented

- **Phonology** (Sec. 1): 31 consonants, 9 vowels, vowel form table (4 series x 9 forms), accent normalization, glottal stop insertion rules
- **Slot I** (Sec. 3.1): Concatenation type detection (Type-1/Type-2), Cc values, Ca shortcut indicators
- **Slot II** (Sec. 3.2): All 8 Stem/Version combinations, optional affix shortcuts, Cs-root and personal-reference root Vv values
- **Slot III** (Sec. 3.3): Root consonant extraction (1-5 consonants)
- **Slot IV** (Sec. 3.4): All Function/Specification/Context combinations (2x4x4 = 32)
- **Slot V** (Sec. 3.5): VxCs affix parsing with degree (0-9) and type (1-3), glottal stop boundaries
- **Slot VI** (Sec. 3.6): Complete Ca complex with pre-generated 3,840 allomorphic forms (Configuration x Extension x Affiliation x Perspective x Essence), bidirectional lookup, gemination rules
- **Slot VII** (Sec. 3.7): VxCs affix parsing with scope over Ca
- **Slot VIII** (Sec. 3.8): Pattern 1 (Valence/Phase/Effect/Level + Mood/Case-Scope) and Pattern 2 (36 Aspects + Mood/Case-Scope)
- **Slot IX** (Sec. 3.9): All 68 cases across 8 groups, including glottal-stop cases 37-68
- **Slot X** (Sec. 3.10): Stress-based Relation detection
- **Illocution/Validation** (Sec. 3.9.3): All 9+9 values with Vk parsing
- **Concatenation** (Sec. 3.1): Type-1/Type-2 compound formatives
- **Ca shortcut formatives** (Sec. 3.2): w/y prefix with 8 Ca forms
- **Cs-root formatives** (Sec. 4.2): Affix Cs as root with degree+context in Vr

### Adjuncts (Sec. 4)

- **Bias adjuncts** (Sec. 4.7): All 61 bias markers
- **Register adjuncts** (Sec. 4.4): DSV, PNT, SPF, EXM, CGT, END
- **Modular adjuncts** (Sec. 4.3): VnCn pairs with scope and aspect
- **Affixual adjuncts** (Sec. 4.1): Single and multiple-affix forms
- **Suppletive adjuncts** (Sec. 4.5): CAR, QUO, NAM, PHR

### Referentials (Sec. 4.6)

- 11 referent categories x 3 effects, combination referentials, biconsonantal forms
- Referent category affixes (Agglomerative, Nomic, Abstract)
- Specialized personal-reference roots

### Other Features

- **Numbers** (Ch. 13): Centesimal system, TNX affix for 11-99, power roots
- **Lexicon**: 4,720 roots and 527 affixes with keyword search and ranking
- **Allomorph engine**: 3,840 Ca forms with 9 substitution rules
- **Composition engine**: Formative construction from grammatical specifications
- **Phonotactic validation**: Consonant cluster and vowel sequence constraints

### Partial

- **Morpho-phonemic script** (Ch. 12): Type definitions for Primary/Secondary/Tertiary/Quaternary characters; SVG rendering skeleton


## Building and Running

Requires Go 1.25+. The repo uses [Nix](https://nixos.org/) to pin the toolchain, but `go` from any source works.

Install the CLIs to `$GOBIN` (typically `~/go/bin`, or `~/.local/bin` if you've set `GOBIN`):

```bash
nix develop              # Enter dev shell with go on PATH
go install ./cmd/ithkuil ./cmd/ithkuil-mcp ./cmd/ithkuil-input
```

Then `ithkuil analyze Malëuţřait` works from anywhere on PATH.

For development:

```bash
go build ./...                              # Build everything
go test ./...                               # Run the test suite
go run ./cmd/ithkuil analyze Malëuţřait     # Run without installing
```

### ASCII Input Method

`ithkuil-input` is a TUI for typing Ithkuil Unicode from ASCII keystrokes. Pending characters are shown dimmed until they resolve into a digraph or are broken by a different vowel:

```bash
ithkuil-input            # interactive raw-mode TUI
echo 't,Maleeut,rqait' | ithkuil-input   # batch: ASCII in, Unicode out
```

| | Char | ASCII | Char | ASCII | Char | ASCII | Char | ASCII | Char | ASCII |
|----------|------|-------|------|-------|------|-------|------|-------|------|-------|
| Umlaut   | ä | `aa` | ë | `ee` | ö | `oo` | ü | `uu` | | |
| Cedilla  | ţ | `t,` | ḑ | `d,` | ļ | `l,` | ç | `c,` | | |
| Háček    | š | `sq` | ž | `zq` | č | `cq` | ň | `nq` | ř | `rq` |
| Underdot | ẓ | `dz` | | | | | | | | |

Vowel runs use right-grouping: `eee`→eë, `eeee`→ëë. Pending characters are shown dimmed until resolved.

### CLI Subcommands

```bash
ithkuil analyze Malëuţřait              # full slot breakdown (above)
ithkuil analyze --short Malëuţřait      # one-line surface / type / gloss
ithkuil analyze --polygraph kala mala   # multi-column slot polygraph

ithkuil compose --stem=S2 --case=ERG ml # build a formative → "emlalo"

ithkuil diff malëuţřait melëuţřait      # slot-by-slot diff
ithkuil diff a b c -- a b d             # aligned sentence diff

ithkuil grammar Ergative --exact        # look up by abbreviation
ithkuil grammar please                  # substring search across all categories
ithkuil grammar --category Case         # list every entry in a category

ithkuil lexicon clown --kind=root       # search the root lexicon
ithkuil lexicon -k=affix system         # search the affix lexicon

ithkuil validate tttest                 # phonotactic checks per word
```

Pass `--lex DIR` globally to override the embedded lexicon with `roots.json` + `affixes.json` from a local directory. Pass `--color=auto|always|never` to `analyze` to control ANSI styling.

## References

- **Morphology v1.3.1** (2023-02-11): The primary reference document
- **Official website**: [ithkuil.net](http://ithkuil.net)
- **Community archive**: [ithkuil.place](https://ithkuil.place)


## Acknowledgments

Ithkuil is written by **John Quijada**, who has been developing the language since the 1970s.

Thanks to **ngoriyev** for the Kotlin codebase [IthkuilGloss](https://github.com/ngoriyev/IthkuilGloss), which was used as a reference for some parts of this code.
