# Ithkuil Grammar Implementation

A Haskell implementation of the grammar of **Ithkuil**, the constructed language created by **John Quijada**. Ithkuil is a philosophical language designed to express deeper levels of human cognition with maximum precision and conciseness, featuring a morpho-phonological system of extraordinary depth and regularity.

This project implements a parser, renderer, and glosser for Ithkuil formatives and adjuncts across all four versions of the language, with the primary focus on **Version IV** (2023, community name *Malëuţřait*). The goal is to serve as a canonical programmatic reference for the language's grammar.

## The Four Versions

| Version | Year | Native Name | Description |
|---------|------|-------------|-------------|
| I | 2004 | Iţkuîl | Original grammar with consonant grades and biliteral roots |
| II | 2007 | ilákš | Revised with tonal vowel system |
| III | 2011 | Elartkha | Major redesign with Designation, Pattern, and Sanction |
| IV | 2023 | Malëuţřait | Current version (morphology document v1.3.1) |

## What This Project Does

Given an Ithkuil word or sentence, the glosser parses it into its morphological components, looks up roots and affixes in the lexicon, and produces a human-readable gloss:

```
$ cabal run ithkuil-gloss -- 'Malëuţřait'
  Malëuţřait  [WFormative]
    Root: -m- = linguistic utterance for communication
    Stem/Version: S1/PRC
    Function/Spec/Context: STA/BSC/EXS
    Affix: -ţř- ëu = SYS () deg 5: A feedback-driven/self-sustaining/autopoietic system based on X
    Case: POS
    GLOSS: 'linguistic utterance for communication'-SYS/5₂-POS
```

The tool provides:
- **Morphological parsing**: Decomposition of formatives into their 10 slot structure
- **Lexicon lookup**: 4,720 roots and 527 affixes from the official lexicon
- **Word type classification**: Formatives, referentials, bias adjuncts, register adjuncts, modular adjuncts, affixual adjuncts, carrier adjuncts, combination referentials
- **Composition**: Building formatives from grammatical specifications
- **Grammar search**: Looking up grammatical categories, roots, and affixes by keyword
- **Sentence glossing**: Multi-word sentence parsing with concatenation chain detection

## V4 Implementation Status

### Fully Implemented

- **Phonology** (Sec. 1): 31 consonants, 9 vowels, vowel form table (4 series x 9 forms), accent normalization, glottal stop insertion rules
- **Slot I** (Sec. 3.1): Concatenation type detection (Type-1/Type-2), Cc values (h, hw, w, y, hl, hr, hm, hn), Ca shortcut indicators
- **Slot II** (Sec. 3.2): All 8 Stem/Version combinations (4 stems x PRC/CPT), optional affix shortcuts (NEG/4, DCD/4, DCD/5), Cs-root Vv values (ëi, eë, ëu, oë), personal-reference root Vv values (ae, ea)
- **Slot III** (Sec. 3.3): Root consonant extraction (1-5 consonants)
- **Slot IV** (Sec. 3.4): All Function/Specification/Context combinations (STA/DYN x BSC/CTE/CSV/OBJ x EXS/FNC/RPS/AMG), Cs-root Vr (degree + context)
- **Slot V** (Sec. 3.5): VxCs affix parsing with degree (0-9) and type (1-3) classification, glottal stop boundary detection for multiple affixes
- **Slot VI** (Sec. 3.6): Complete Ca complex with pre-generated 3,840 allomorphic forms from component tables (Configuration x Extension x Affiliation x Perspective x Essence), bidirectional lookup, gemination rules for Slot V presence
- **Slot VII** (Sec. 3.7): VxCs affix parsing with scope over Ca
- **Slot VIII** (Sec. 3.8): Both patterns fully implemented
  - Pattern 1: Valence (9), Phase (9), Effect (6), Level (9) + Mood/Case-Scope
  - Pattern 2: All 36 Aspects + Mood/Case-Scope
  - Mood: FAC, SUB, ASM, SPC, COU, HYP (6 values)
  - Case-Scope: CCN, CCA, CCS, CCQ, CCP, CCV (6 values)
  - Both Pattern 1 (h/hl/hr/hm/hn/hň) and Pattern 2 (w/y/hw/hrw/hmw/hnw/hňw) Cn consonants
- **Slot IX** (Sec. 3.9): All 68 cases across 8 groups (Transrelative, Appositive, Associative, Adverbial, Relational, Affinitive, Spatio-Temporal I, Spatio-Temporal II), including glottal-stop cases 37-68
- **Slot X** (Sec. 3.10): Stress-based Relation detection (penultimate = UNFRAMED + Vc, ultimate = UNFRAMED + Vk, antepenultimate = FRAMED + Vc)
- **Illocution** (Sec. 3.9.3): All 9 values (ASR, DIR, DEC, IRG, VER, ADM, POT, HOR, CNJ)
- **Validation** (Sec. 3.9.3): All 9 values (OBS, REC, PUP, RPR, USP, IMA, CVN, ITU, INF)
- **Vk parsing** (Sec. 3.9.3.3): Series 1 (ASR + Validation) and Series 2 (non-ASR illocutions)
- **IVL affix** (-nļ-): Illocution+Validation as VxCs affix for framed/concatenated formatives
- **Concatenation** (Sec. 3.1): Hyphen-joined concatenation chains, Type-1/Type-2 detection, VF format vowels (no glottal stop), concatenated formative stress rules
- **Ca shortcut formatives** (Sec. 3.2): w/y prefix Vv shortcuts with 8 specific Ca forms (al, atļ, av, aj, ař, adl, ar, ad series)
- **Cs-root formatives** (Sec. 4.2): Specialized formative structure where an affix Cs replaces the root, with degree+context in Vr

### Adjuncts (Sec. 4)

- **Bias adjuncts** (Sec. 4.7): All 61 bias markers with consonant forms, parsing, and human-readable glosses (e.g., DOL = "Ow! Ouch!", FSC = "Cool! Wow!", IRO = "Just great!")
- **Register adjuncts** (Sec. 4.4): DSV, PNT, SPF, EXM, CGT initial forms (ha/he/hi/ho/hu) and final forms (hai/hei/hiu/hoi/hui), END final-only form (hüi)
- **Modular adjuncts** (Sec. 4.3): VnCn pair parsing with scope prefix (w/y), final vowel as Aspect or Vh scope marker, stress-dependent interpretation
- **Affixual adjuncts** (Sec. 4.1): Single-affix (V-C-V pattern) and multiple-affix (Cs-Vx-Cz-VxCs... pattern) with Cz/Vz scope markers
- **Suppletive adjuncts** (Sec. 4.5): CAR (hl), QUO (hm), NAM (hn), PHR (hň) with case vowel parsing

### Referentials (Sec. 4.6)

- **Single/Dual referentials** (Sec. 4.6.1): 11 referent categories (1m, 2m, 2p, ma, pa, mi, pi, Mx, Rdp, Obv, PVS) x 3 effects (NEU, BEN, DET) = 33 C1 forms, with case marking
- **Combination referentials** (Sec. 4.6.2): Specification (x/xt/xp/xx) + VxCs affixes + case stacking
- **Biconsonantal referential forms**: th/ph/kh (Rdp), ll/rr/řř (Obv), mm/nn/ňň (PVS)
- **Referential cluster decomposition**: Greedy left-to-right matching of referent consonant clusters
- **Referent category affixes** (Sec. 4.6): Agglomerative (-ļ-/-tļ-), Nomic (-ç-/-x-), Abstract (-w/-y) prefix/suffix detection on C1 clusters
- **Specialized Personal-Reference Roots** (Sec. 4.6.4): ae/ea Vv markers for referential-as-formative

### Special Affix Types

- **Case-accessor affixes** (Sec. 3.9.2): Type-1 (sw/sy), Type-2 (zw/zy), Type-3 (čw/čy) with case vowel encoding
- **Inverse case-accessor affixes**: Type-1 (šw/šy), Type-2 (žw/žy), Type-3 (jw/jy)
- **Case-stacking affixes**: lw/ly consonants
- **Ca-stacking**: Vx = üö triggers Ca-complex interpretation of Cs
- **Y-series glottalization**: Automatic vowel glottalization for y-series case affixes

### Numbers (Sec. 5 / Ch. 13)

- Centesimal (base-100) number system with digit roots 0-10
- TNX affix (-rs-) for numbers 11-99
- Power roots for 100^n (gz, pc, kẓ, čg)
- Number formative construction with stem/version variants
- Month affixes (1-12) and day-of-week affixes (1-7)

### Other Features

- **Lexicon**: 4,720 roots and 527 affixes loaded from JSON, with keyword search and ranking
- **Allomorph engine**: Pre-generates all 3,840 Ca complex forms with 9 allomorphic substitution rules, stored as bidirectional lookup maps
- **Composition engine**: Builds formatives from grammatical specifications, with automatic slot filling and stress application
- **Grammar search**: Keyword search across all grammatical categories with ranked results
- **Phonotactic validation**: Consonant cluster length limits, invalid cluster detection

### Partial / In Progress

- **Morpho-phonemic script** (Ch. 12): Type definitions for Primary/Secondary/Tertiary/Quaternary characters; SVG rendering skeleton
- **V3 parser** (Elartkha, 2011): Grammar types defined, Slot II/IV parsing; case and Ca parsing not yet implemented
- **V2 parser** (ilákš, 2007): Basic tone detection and V-C-V-C pattern recognition
- **V1 parser** (Iţkuîl, 2004): Consonant grade detection (8 grades) and vocalic infix parsing

## Project Structure

```
src/Ithkuil/
  Phonology.hs      31 consonants, 9 vowels, vowel form table
  Grammar.hs         All morphological types: 68 cases, configurations, stems, versions, Ca
  Parse.hs           Formative parsing: conjunct splitting, slot-by-slot extraction
  FullParse.hs       Complete parsing with stress, VnCn, aspects
  Allomorph.hs       Ca complex: 3,840 pre-generated forms with allomorphic substitutions
  Render.hs          Formative-to-text rendering
  Gloss.hs           Human-readable glossing at three precision levels
  WordType.hs        Word classification, parsing, and glossing pipeline
  Adjuncts.hs        61 bias markers, registers, suppletive adjuncts
  Referentials.hs    11 referent categories x 3 effects, cluster decomposition
  Numbers.hs         Centesimal number system
  Concatenation.hs   Type-1/Type-2 compound formatives
  Compose.hs         Grammar lookup, formative composition, root/affix search
  Lexicon.hs         JSON lexicon loading (roots + affixes)
  Validation.hs      Phonotactic constraint checking
  Script.hs          Morpho-phonemic writing system (in progress)
  V1/                Version I parser (2004)
  V2/                Version II parser (2007)
  V3/                Version III parser (2011)

app/Gloss.hs         Interactive glossing tool (CLI)
test/Main.hs         358 tests covering all implemented features
data/
  roots.json         4,720 V4 root entries
  affixes.json       527 V4 affix entries
  roots_v3.json      V3 lexicon
  roots_v1.json      V1 lexicon
reference/
  grammar/           Markdown reference documentation for all chapters
  grammar/morphology_v1.3.1.pdf   Authoritative grammar specification
```

## Building and Running

Requires [Nix](https://nixos.org/) for reproducible builds:

```bash
nix-shell               # Enter dev shell with GHC + cabal
cabal update             # Fetch package index (first time only)
cabal build              # Build library + executable
cabal test               # Run test suite (358 tests)
cabal run ithkuil-gloss  # Launch interactive glosser
```

### Interactive Glosser Commands

```
> Malëuţřait              # Gloss a word
> wambalásk               # Parse any V4 formative
> :root -m-               # Look up a root
> :affix -ţř-             # Look up an affix
> :search clown           # Search roots by keyword
> :compose S1 PRC -m- STA BSC EXS THM   # Build a formative
> :grammar Case            # Browse grammar tables
```

## Authoritative References

- **Morphology v1.3.1** (2023-02-11): The primary reference document, included as `reference/grammar/morphology_v1.3.1.pdf`
- **Official website**: [ithkuil.net](http://ithkuil.net)
- **Community archive**: [ithkuil.place](https://ithkuil.place)

## Acknowledgments

Ithkuil is the creation of **John Quijada**, who has been developing the language since the 1970s. The depth, coherence, and intellectual ambition of the grammar are extraordinary. This implementation attempts to faithfully encode the rules described in the morphology document, and any errors in the implementation are the responsibility of the implementors, not the language designer.

This project draws on reference implementations by **Christian Oudard** ([mamkait](https://github.com/christian-oudard/mamkait), Haskell) and **ngoriyev** ([IthkuilGloss](https://github.com/ngoriyev/IthkuilGloss), Kotlin).

## License

This is an educational and research project. The Ithkuil language and its grammar are the intellectual property of John Quijada. The lexicon data files (`data/roots.json`, `data/affixes.json`) are derived from the official Ithkuil lexicon.
