# Chapter 12: The Writing System

**Writing System for New Ithkuil** — Version 1.0, December 2022

The Ithkuil writing system is **morpho-phonemic**: characters convey both phonological and morphological (grammatical) information. The reader constructs the intended words based on their knowledge of the grammar.

> **Note:** The actual character glyphs are visual shapes that cannot be represented in plain text. This document describes the structure, rules, and combinatorics of the system. Refer to `script_v1b.pdf` for the visual glyph charts. Handwritten forms (shown in blue in the PDF) have diagonal segments curved inward or outward so that there are no diagonal end-points, only vertical or horizontal end-points.

## 12.0 Sequence of Written Characters for Formatives

A formative is written as a sequence of up to five character types:

| Primary Character | Secondary Character | Secondary Character(s) | Secondary Character(s) [rotated] | Tertiary Character | Quaternary Character |
|---|---|---|---|---|---|
| Relation, Concatenation Status, Stem, Specification, Function, Version, Plexity, C_A | C_R (Main Root) (Slot V) | C_S (Slot V) | C_S (Slot VII) [laterally rotated 180 degrees] | Valence, Phase, Effect, Aspect, Level | V_C/V_K plus Mood and/or Case-Scope |

For a **Concatenated Pair** of formatives, each formative is simply written separately, first the concatenated formative, then the parent formative. There is no distinction made between the two except that the subscript diacritic on the word-initial Primary Character of the concatenated formative shows the concatenation status.

## 12.1 Primary Characters

A Primary Character is word-initial and shows V_R Specification, Function, and Context, plus V_V Version and Stem, plus all C_A information.

The Primary Character is a composite glyph built from several overlapping zones:

### Structure

The Primary Character is built around a **diagonal bar** (the core stroke). Various features are encoded in different zones:

- **Super-posed diacritic** (above): 4 Contexts (EXS, FNC, RPS, AMG)
- **Upper-left zone** (4 Perspectives x 6 Extensions = 24 combinations)
- **Upper-right zone** (4 Affiliations x 2 Essences = 8 combinations)
- **Lower-left zone** (4 Specifications: BSC, CTE, CSV, OBJ)
- **Lower-right zone** (4 Stems x 2 Functions x 2 Versions x 2 Plexities = 64 combinations)
- **Under-posed diacritic** (below): 10 Configurations (PX, SS, SC, SF, DS, DC, DF, FS, FC, FF)
- **Subscript diacritic** (beneath, on parent/standalone only): 3 Relations x 2 Concatenations

### 3 Relations (placed beneath the Primary Character of a parent or standalone formative only)

| Relation | Diacritic |
|---|---|
| Noun | (none) |
| Unframed Verb | diamond/dot |
| Framed Verb | horizontal bar |

### 2 Concatenations (placed beneath the Primary Character of a concatenated formative only)

| Type | Diacritic |
|---|---|
| Type-1 Concatenation | vertical wedge |
| Type-2 Concatenation | zigzag |

### 4 Contexts (super-posed diacritic)

| Context | Diacritic |
|---|---|
| EXS (Existential) | diamond/dot |
| FNC (Functional) | horizontal bar |
| RPS (Representational) | angled stroke |
| AMG (Amalgamative) | angled stroke (reversed) |

### 4 Specifications

| Specification | Shape |
|---|---|
| BSC (Basic) | large curve or straight |
| CTE (Contential) | small curve |
| CSV (Constitutive) | angular |
| OBJ (Objective) | angular (reversed) |

### 4 Perspectives x 6 Extensions

Rows: M (Monadic), G (Agglomerative), N (Nomic), A (Abstract)
Columns: DEL (Delimitive), PRX (Proximal), ICP (Inceptive), ATV (Attenuative), GRA (Graduative), DPL (Depletive)

24 distinct glyph shapes encode these combinations.

### 4 Affiliations x 2 Essences

| Essence | CSL | ASO | VAR | COA |
|---|---|---|---|---|
| NRM (Normal) | thin stroke | thick stroke | vertical wedge | horizontal bar |
| RPV (Representative) | arrow | upward wedge | hook | crescent |

### 10 Configurations

Plexity (U/M vs. D) is shown with Stem, Function & Version in the lower-right zone.

| Config | Shape |
|---|---|
| PX (Processual) | plain diagonal |
| SS (Similar-Separate) | double diagonal |
| SC (Similar-Connected) | diagonal + wedge |
| SF (Similar-Fused) | diagonal + bar |
| DS (Dissimilar-Separate) | angular diagonal |
| DC (Dissimilar-Connected) | curved form |
| DF (Dissimilar-Fused) | hooked form |
| FS (Fuzzy-Separate) | barred diagonal |
| FC (Fuzzy-Connected) | double-barred |
| FF (Fuzzy-Fused) | spiral form |

### Stem/Function/Version/Plexity Combinations

The lower-right zone encodes: 4 Stems (1, 2, 3, 0) x 2 Functions (STA, DYN) x 2 Versions (PRC, CPT) x 2 Plexities (U/M, D) = 64 distinct forms.

These are shown in two tables in the PDF (one for STA, one for DYN), each with columns for PRC-U/M, PRC-D, CPT-U/M, CPT-D and rows for Stems 1-3 and 0.

### Primary Character Elision

Any word-initial Primary Character consisting of a plain diagonal bar (i.e., being "default" CSL/UPX/DEL/M/NRM/PRC/STA/BSC/EXS/Stem.1 on a nominal formative) may be elided if the word is in sentence-initial position.

If the word is an unconcatenated verb, the diagonal bar may also be elided and the underposed dot or horizontal bar showing verbal status will instead be placed at the mid-line to the left of the first character (i.e., the Secondary character), but only as long as the word is sentence-initial.

## 12.2 Secondary Characters

Consonantal characters used for displaying C_R and C_S. The C_R character follows the word-initial Primary character.

### 12.2.1 Core Secondary Characters

The 28 core forms each have a "top" and "bottom" end that take extensions for consonant clusters. Each consonant has both a print and handwritten form.

| Voiceless | Voiced | Voiceless | Voiced |
|---|---|---|---|
| p | b | f | v |
| t | d | ţ | ḑ |
| k | g | s | z |
| x | — | š | ž |
| — | — | c | ż |
| — | — | č | j |
| — | — | ç | h |
| l | r | ļ | ř |
| m | n | ň | — |
| w | y | — | — |

### 12.2.2 Consonantal Extensions to Secondary Characters

Extensions to the upper "end" of a core consonant character add a **preceding** consonant. Extensions to the lower "end" add a **following** consonant. Additional consonants may be added by applying extensions to a **placeholder character**.

For each of the 28 consonants, there are three extension orientations:
- **Vertical** (4 forms: print-upper, print-lower, handwritten-upper, handwritten-lower)
- **Diagonal** (2 forms: print, handwritten)
- **Horizontal** (4 forms: print-upper, print-lower, handwritten-upper, handwritten-lower)

The voiceless stops (p, t, k) and their voiced counterparts (b, d, g) have the most visually distinct extension sets.

Additionally, there are special extensions for:
- **Gemination of core consonant** (doubling the core consonant)
- **Gemination of other extension** (doubling an extension consonant)

## 12.2.3 Using Secondary Characters to Show V_xC_S Affixes

Use the Secondary Characters with their extensions for any Slot V C_S character(s), placed immediately following the C_R character.

### Degree Diacritics

The character takes an underposed diacritic to show V_x Degree:

| Degree | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | C_A stacking | Degree "0" |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Super-posed | dot | hook | slash | curl | bar/backslash | crescent | wedge | zigzag | arc | cup | arch |
| Under-posed | dot | hook | curl | curl | bar/backslash | crescent | wedge | dash | crescent | cup | arch |

### Affix Type Diacritics

- **Type-1**: no additional diacritic
- **Type-2**: super-posed dot diacritic above the character
- **Type-3**: super-posed bar diacritic above the character

### Slot VII V_xC_S Affixes

Place any Slot VII C_S character(s) immediately after any Slot V C_S character(s) but **rotate the character 180 degrees** (i.e., they are upside-down but not horizontal mirror-images). Type-2 and Type-3 Slot VII affixes use the same superposed dot or bar diacritics.

### Affixual Scoping

The written language handles affix scoping with the sequential ordering of Secondary C_S character(s) within a formative. There is one diacritic mark — **a dot placed along the right side** of a Secondary C_S character — to show that the affix has scope over the entire formative as a whole, including Valence, Mood or Case, Illocution & Validation, etc.

## 12.2.4 Rotated Secondary Characters

Identical to standard Secondary characters except laterally rotated 180 degrees. Used for:

1. **Slot VII V_xC_S affixes** (placed after Slot V affixes or after the C_R character if no Slot V affixes)
2. **Specialized C_S-Roots** (Sec. 4.2 of the Design Document)
3. **Specialized Personal-Reference Roots** (Sec. 4.6.4 of the Design Document)

### Specialized C_S-Roots

Roots where the Slot III C_R infix is replaced by a the C_S consonantal form of a V_xC_S affix. Written using rotated Secondary Characters in place of the initial (non-rotated) Secondary Character representing C_R. The use of a rotated Secondary Character immediately following the word-initial Primary Character indicates to the reader that the word is a Specialized C_S-Root.

The Degree of the C_S consonantal form is shown by the same nine underposed diacritics used for standard V_xC_S affixes. Note: no Type-1 vs. Type-2 vs. Type-3 affix-type distinction is made in a Specialized C_S-Root.

### Specialized Personal-Reference Roots

Roots where the Slot III C_R infix is replaced by a single or combination Referential affix. Written using rotated Secondary Characters in place of the initial (non-rotated) Secondary Character representing C_R, exactly the same as a Specialized C_S-Root, except that the rotated Secondary Character carries a **super-posed dot diacritic**.

## 12.3 Tertiary Characters

A composite character placed after all Secondary Characters, indicating Valence, Aspect, Phase, Effect, and Level.

### Character Structure

The Tertiary Character has three components:
- **Left segment** (vertical): Valence
- **Upper-right segment**: Aspect, Phase, or Effect (upper)
- **Lower-right segment**: Aspect, Phase, or Effect (lower, only if two aspects are shown)

The horizontal Valence segment is mandatory. A Phase or Effect segment is also mandatory unless there are two aspects shown (one in upper half, one in lower half). A plain default CTX/MNO character is elided.

### Effect

| Effect | Form |
|---|---|
| neutral | simple V-shape |
| 1/BEN | V with right hook |
| 2/BEN | V with left hook |
| 3/BEN | V with both hooks |
| SLF/BEN | V with inner marks |
| UNKNOWN | V with loop |
| SLF/DET | inverted V with loop |
| 3/DET | inverted V with both hooks |
| 2/DET | inverted V with left hook |
| 1/DET | inverted V with right hook |

### Phase

| Phase | Form |
|---|---|
| PCT (Punctual) | two vertical bars |
| ITR (Iterative) | bar + hook |
| REP (Repetitive) | bar + curl |
| ITM (Intermittent) | bar + zigzag |
| RCT (Recurrent) | arc + bar |
| FRE (Frequentative) | zigzag pair |
| FRG (Fragmentative) | hook pair |
| VAC (Vacillative) | dot + hook |
| FLC (Fluctuative) | double hook |

### Valence

| Valence | Form |
|---|---|
| MNO (Monoactive) | plain bar |
| PRL (Parallel) | bar + small curve |
| CRO (Complementary) | bar + medium curve |
| ICP (Reciprocal) | bar + large curve |
| CPL (Completive) | bar + S-curve |
| DUP (Duplicative) | double curve |
| DEM (Demonstrative) | curve + hook |
| CNG (Contingent) | hook pair |
| PTI (Participative) | S-curve |

### Aspect (36 aspects in 4 rows of 9)

Row 1: RTR, PRS, HAB, PRG, IMM, PCS, REG, SMM, ATP
Row 2: RSM, CSS, PAU, RGR, PCL, CNT, ICS, EXP, IRP
Row 3: PMP, CLM, DLT, TMP, XPD, LIM, EPD, PTC, PPR
Row 4: DCL, CCL, CUL, IMD, TRD, TNS, ITC, MTV, SQN

### Level

Shown by diacritic on the Tertiary Character:
- Super-posed = Absolute
- Under-posed = Relative

| Level | Diacritic |
|---|---|
| MIN (Minimal) | dot |
| SBE (Subequative) | zigzag |
| IFR (Inferior) | wedge |
| DFC (Deficient) | hook |
| EQU (Equative) | bar/backslash |
| SUR (Surpassive) | crescent |
| SPL (Superlative) | slash |
| SPQ (Superequative) | reverse zigzag |
| MAX (Maximal) | horizontal bar |

## 12.4 Quaternary Characters — V_C/V_K Characters

Used for displaying V_C Case and V_K Illocution+Validation, as well as C_M Mood and C_C Case-Scope. V_C and V_K are shown by extensions to the top and bottom ends of a plain vertical bar. Mood and Case-Scope are indicated by diacritics. A Quaternary Character is placed immediately after any Tertiary Character.

### Case Type (top extension)

| Case Type | Extension |
|---|---|
| TRANSRELATIVE | plain bar |
| APPOSITIVE | curve |
| ASSOCIATIVE | hook |
| ADVERBIAL | bracket |
| RELATIONAL | double hook |
| AFFINITIVE | curve + hook |
| SPATIO-TEMPORAL I | zigzag |
| SPATIO-TEMPORAL II | double zigzag |

### Case Number (bottom extension)

Cases 1-9 within each type are shown by bottom extensions (1 = plain, up to 9).

**Note:** The RLT, VOC, NAV, and PLM cases (which do not use Vowel Sequence Tier No. 8) are written using the Case No. 9 extension, not No. 8.

### Illocution (top extension, for V_K)

| Illocution | Extension |
|---|---|
| ASR (Assertive) | plain bar |
| DIR (Directive) | curve |
| DEC (Declarative) | hook |
| IRG (Interrogative) | bracket |
| VRF (Verificative) | double hook |
| ADM (Admonitive) | curve + hook |
| POT (Potentiative) | zigzag |
| HOR (Hortative) | angular |
| CNJ (Conjectural) | crescent |

### Validation (bottom extension, used with ASR Illocution only)

| Validation | Extension |
|---|---|
| OBS (Observational) | plain bar |
| REC (Recollective) | curve |
| PUP (Purportive) | hook |
| RPR (Reportive) | bracket |
| IMA (Imaginary) | double hook |
| CVN (Conventional) | curve + hook |
| ITU (Intuitive) | zigzag |
| INF (Inferential) | angular |
| USP (Unspecified) | crescent |

### 12.4.1 Diacritics Used with Quaternary Characters

#### Case-Scope (under-posed diacritic)

| Case-Scope | Diacritic |
|---|---|
| CCN (Natural) | dot |
| CCA (Antecedent) | bar/backslash |
| CCS (Subaltern) | hook |
| CCQ (Qualifier) | crescent |
| CCP (Precedent) | arc |
| CCV (Successive) | horizontal bar |

#### Mood (super-posed diacritic)

| Mood | Diacritic |
|---|---|
| FAC (Factual) | (none/zero) |
| SUB (Subjunctive) | bar/backslash |
| ASM (Assumptive) | hook |
| SPC (Speculative) | crescent |
| COU (Counterfactive) | arc |
| HYP (Hypothetical) | horizontal bar |

#### Case-Accessor Affixes

Composed of a Quaternary Character indicating Case, accompanied by special diacritics:
- **Regular Case-Accessor**: under-posed curve
- **Inverse Case-Accessor**: under-posed arch

#### Type-2 or Type-3 Case-Accessor Affixes

- Type-2: super-posed dot diacritic above the Quaternary character
- Type-3: super-posed bar diacritic above the Quaternary character

#### Distinguishing Slot V vs. Slot VII Case-Accessor Affixes

If necessary, the Slot VII case-accessor adds a dot to the diacritic (both regular and inverse forms).

#### Case-Stacking

A second case, with scope over the first, is shown by simply adding a 2nd Quaternary Character immediately after the first.

### 12.4.2 Alternative — Showing V_C/V_K Using Diacritics on the C_R Character

If Mood and Case-scope are default FAC/CCN (so that there are no diacritics above or below the Quaternary V_C/V_K Character), then the option exists to dispense with the Quaternary character and instead show V_C or V_K on the C_R consonantal root character using superposed and underposed diacritics.

#### Showing V_C Case (on C_R character)

Case Type shown by super-posed diacritic (same shapes as Quaternary top extensions):

| | TRANSRELATIVE | APPOSITIVE | ASSOCIATIVE | ADVERBIAL | RELATIONAL | AFFINITIVE | SPATIO-TEMP I | SPATIO-TEMP II |
|---|---|---|---|---|---|---|---|---|
| Super-posed | dot | bar | curve | arch | arrow | zigzag | hook | crescent |
| Under-posed | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 / 9 |

#### Showing V_K Illocution/Validation (on C_R character)

| | ASR | DIR | DEC | IRG | VRF | ADM | POT | HOR | CNJ |
|---|---|---|---|---|---|---|---|---|---|
| Super-posed | dot | bar | curve | arch | arrow | zigzag | hook | crescent | — |
| Under-posed | OBS | REC | PUP | RPR | IMA | CVN | ITU | INF | USP |

### 12.4.3 Showing Referentials

To show a referential, use a Quaternary Character followed by a Secondary Character (with extensions if needed) to indicate the specific personal referent(s) with a superposed horizontal bar diacritic on the Secondary Character. Dual-referent adjuncts are written as two referentials next to each other. To show case-stacking on a Referential, place the 2nd Quaternary Character AFTER the Secondary Character (i.e., so that the Secondary Character is sandwiched between the two Quaternary Characters).

## 12.5 Bias Characters

Two base forms (one pair of mirrored S-shapes) encode all bias adjuncts. Each bias is shown as a variant of these base forms.

If sentence-initial, place immediately before the first formative or referential without a space between them. If sentence-final, place immediately after the last formative or referential without a space. If used as a standalone sentence, separate it from the preceding and/or following sentences by a space.

### Bias List

| Bias | Abbr. | Bias | Abbr. | Bias | Abbr. | Bias | Abbr. |
|---|---|---|---|---|---|---|---|
| Accidental | ACC | Fascination | FSC | Perplexity | PPX | Sarcastic | SAT |
| Archetypal | ACH | Gratification | GRT | Propitious | PPT | Suggestive | SGS |
| Admiration | ADM | Indignation | IDG | Prosaic | PPV | Skeptical | SKP |
| Annoyance | ANN | Infatuation | IFT | Pessimistic | PES | Solicitative | SOL |
| Anticipation | ANP | Implication | IPL | Perceptive | PSC | Stupefaction | STU |
| Approbation | APB | Impatience | IPT | — | — | Trepidation | TRP |
| Apprehension | APH | Ironic | IRO | Reactive | RAC | Vexation | VEX |
| Arbitrary | ARB | Inspiration | ISP | Reflective | RFL | — | — |
| Attentive | ATE | Invidious | IVD | Renunciative | RNC | — | — |
| Comedic | CMD | Mandatory | MAN | Repulsive | RPU | — | — |
| Contensive | CNV | Optimal | OPT | Revelative | RVL | — | — |
| Coincidental | COI | — | — | — | — | — | — |
| Corrective | CRP | Desperative | DES | — | — | — | — |
| Corroborative | CRR | Diffident | DFD | — | — | — | — |
| Contemptive | CTP | Disconcertive | DIS | — | — | — | — |
| Contrite | CTV | Delectative | DLC | — | — | — | — |
| Derisive | DCC* | Dolorous | DOL | — | — | — | — |
| Dejection | DEJ | Disapprobative | DPB | — | — | — | — |
| Dismissive | DRS | Dubitative | DUB | — | — | — | — |
| Euphemistic | EUH | Euphoric | EUP | — | — | — | — |
| Exasperation | EXA | Exigent | EXG | — | — | — | — |
| Experiential | EXP | Fortuitous | FOR | — | — | — | — |
| Presumptuous | PSM* | — | — | — | — | — | — |

\* DCC and PSM Biases are distinguished from the ACC and FSC biases by use of a dot diacritic.

Three unassigned bias slots remain in the system.

## 12.6 Showing Register

Register markers are placed before and after a phrase. Each register has four modes: Standard, Alphabetic, Transcriptive, and Transliterative.

| Register | Standard (open/close) | Alphabetic (open/close) | Transcriptive* (open/close) | Transliterative* (open/close) |
|---|---|---|---|---|
| Narrative | diamond pair | stacked diamond pair | stacked diamond pair (with extra dots) | split diamond pair |
| Discursive | double diamond | double dot | triple dot | split double diamond |
| Parenthetical | diamond + check | stacked diamond + hook | stacked diamond + caret | split diamond + V |
| Cogitant | diamond + S-curve | stacked diamond + C-curve | double diamond + S-curve | split diamond + S-curve |
| Exemplificative | diamond + tilde | stacked diamond + hook-tilde | split diamond + tilde | split filled diamond + tilde |
| Specificative | double diamond + tilde | stacked diamond + S-tilde | split double diamond + S-tilde | split filled double diamond + tilde |

### Transcriptive & Transliterative Modes

Being a morpho-phonemic writing system, the script does not normally represent adjuncts. A written passage may have different spoken interpretations by a reader (i.e., it is up to the reader whether to utilize adjuncts or not when reading aloud).

**Transcriptive Mode** indicates that the word/phrase inside the markers is to be read exactly as standardly written, i.e., without using adjuncts (other than carrier adjuncts). It is also used to indicate the functions of both a Quotative Adjunct (Sec. 4.5.2) and a Phrasal Adjunct (Sec. 4.5.4).

**Transliterative Mode** indicates the word/phrase within the markers is one of: (1) a phonemic rendering of an adjunct written using Secondary Characters alphabetically, or (2) a Tertiary Character representing a Modular Adjunct, to be read first the top "half", then the bottom "half". It is also used to indicate the function of a Naming Adjunct (Sec. 4.5.3).

### Showing Carrier Adjuncts/Stems

Use the appropriate register markers in Alphabetic mode, and insert a Quaternary Case character between the initial register marker and the first Secondary alphabetic character. A full carrier stem may precede the alphabetic register clause per standard rules of writing, or, as a shortcut, place the primary, any tertiary, and quaternary characters immediately after the alphabetic register marker before the first Secondary alphabetic character.

## 12.7 Alphabetic Writing

Use Secondary Characters and their extensions plus a **placeholder character** for alphabetic writing of proper names and foreign words, preceded and followed by the appropriate double-dot diacritic (shown in the Section on writing Register Adjuncts).

### Vowel Diacritics

Vowels are shown as diacritics placed above the character for a preceding vowel, below for a following vowel. Use the placeholder character for standalone vowels if necessary.

| Vowel | a | ä | e | ë | i | o | ö | u | ü |
|---|---|---|---|---|---|---|---|---|---|
| Super-posed | dot | bar | arrow | zigzag | bar/backslash | hook | crescent | slash | wedge |
| Under-posed | dot | bar | hook | curve | bar/backslash | hook | crescent | bar-angle | vertical |

### 2-Vowel Conjuncts/Diphthongs

Show by superposing the first vowel diacritic and underposing the second vowel diacritic on the placeholder character. To show a single vowel between two single consonants, use the placeholder character with the two consonant extensions at top and bottom, with the vowel diacritic placed along the right side of the character.

**Option without placeholder:** To show a two-vowel conjunct/diphthong *preceding* a full-consonant conjunct, place the first vowel diacritic above the Secondary character and the second vowel diacritic along the right side. For a *following* conjunct, place the first vowel diacritic along the right side and the second below.

### Indicating Stress

Penultimate stress is unmarked. Otherwise, the stressed vowel should be shown on a stand-alone **plain vertical bar** instead of the usual placeholder character.

### Placeholder Characters

- **Standard placeholder**: zigzag form (for morpho-phonemic writing — used to carry consonantal extensions for additional consonants)
- **Alphabetic placeholder**: angular form (for alphabetic writing — used with vowel diacritics)

### Additional Characters for Alphabetic Writing

A special Z-shaped character is used in multiple ways to represent foreign words/sounds:

#### Secondary Vocalic Articulations

Extensions on the Z-shape indicate:
- **Long Vowel**: plain extension
- **Nasalization**: dot extension
- **Pharyngealization**: double dot
- **Breathy Voice/Whispered**: triple dot
- **Creaky Voice**: hook extension

The top bar of the Z-shape may take a Secondary Character consonantal extension. Vowel diacritics may be applied above and below.

#### Tones

| Tone | Mark |
|---|---|
| High | left slash |
| Mid | (unmarked) |
| Low | right slash |
| Rising | low curve |
| Falling | high curve |

Combination tones use two markers (e.g., Low-Rising, Falling-Rising).

#### Special Bottom Extensions (on placeholder character)

- **Glottal Stop**: special hooked bottom extension
- **Ejective Consonant**: special barred bottom extension
- **Velarized/Pharyngealized Consonant**: special doubled bottom extension

## 12.8 Writing Numerals

The numeral system uses 10 base characters (0-9), with extensions for higher place values:

### Base Digits (0-9)

Each digit has a distinct cursive glyph form (print and handwritten variants shown in the PDF).

### Extensions

- **Top-right extensions**: hundreds (+100 through +900), 9 distinct forms
- **Bottom-left extensions**: tens (+10 through +90), 9 distinct forms
- **Internal diacritics** (placed inside the top-left quarter of the 1-through-10 symbols): thousands (+1000 through +9000), using the same 9 shapes as the underposed diacritics

**Example**: 7268 would be the digit-7 character with a +200 top-right extension, a +60 bottom-left extension, and an +8000 internal diacritic (displayed in the PDF as a composed single character).

## 12.9 Example

The PDF shows the word **Wezvwaušburdóu** written in the script:
- [default C_A]-Stem2-'fox'-GEO2/2-REA1/9-ADM
- *'Be careful, your fork is actually a fennec.'*
