# Ithkuil Parser Implementation Plan

## Overview

This document outlines the plan to implement parsers for all four versions of Ithkuil, enabling glossing and morphological analysis of each version's native word for "Ithkuil":

| Version | Year | Native Name | Root | Core Structure |
|---------|------|-------------|------|----------------|
| I | 2004 | Itkuil | -K-L- | Consonant grades, vocalic infixes |
| II | 2007 | ilaks | -L- | Tones, simplified phonology |
| III | 2011 | Elartkha | -L- | Similar to V4 but with Designation/Sanction |
| IV | 2023 | Maleutrait | -M- | 10-slot formative, Ca complex |

---

## Phase 1: Complete V4 Parser (Current Focus)

### Current Status
- Basic parsing works for consonant-initial formatives
- Lexicon loading from `data/roots.json` (4717 roots)
- Slot II (Vv), Slot III (Cr), Slot IV (Vr) parsing functional

### Remaining V4 Work

#### 1.1 Complete Ca Complex Parsing (Slot VI)

The Ca complex encodes 5 categories in agglutinated form:
- **Affiliation** (4): CSL, ASO, COA, VAR
- **Configuration** (20): UNI, DSS, DSC, DSF, etc.
- **Extension** (6): DEL, PRX, ICP, ATV, GRA, DPL
- **Perspective** (4): M, G, N, A
- **Essence** (2): NRM, RPV

```haskell
-- New types needed in Grammar.hs
data ParsedCa = ParsedCa
  { caConfig      :: Configuration
  , caAffiliation :: Affiliation
  , caPerspective :: Perspective
  , caExtension   :: Extension
  , caEssence     :: Essence
  }

-- Parse Ca consonant patterns
parseCa :: Text -> Maybe ParsedCa
```

#### 1.2 Case Vowel Parsing (Slot IX)

Complete the 68 case patterns in `casePatterns`:
- Transrelative (9): THM, INS, ABS, AFF, STM, EFF, ERG, DAT, IND
- Appositive (9): POS, PRP, GEN, ATT, PDC, ITP, OGN, IDP, PAR
- Associative (9): APL, PUR, TRA, DFR, CRS, TSP, CMM, CMP, CSD
- Adverbial (9): FUN, TFM, CLA, RSL, CSM, CON, AVR, CVS, SIT
- Relational (8): PRN, DSP, COR, CPS, COM, UTL, PRD, RLT
- Affinitive (8): ACT, ASI, ESS, TRM, SEL, CFM, DEP, VOC
- Spatio-Temporal I (8): LOC, ATD, ALL, ABL, ORI, IRL, INV, NAV
- Spatio-Temporal II (8): CNR, ASS, PER, PRO, PCV, PCR, ELP, PLM

#### 1.3 Affix Parsing (Slots V and VII)

```haskell
-- Affix vowel determines degree (1-9)
-- Affix consonant determines type
parseAffix :: Text -> Text -> Maybe Affix
parseAffixes :: [Text] -> [Affix]
```

#### 1.4 Stress Detection

Stress position determines interpretation of final vowel:
- Penultimate: Case (Vc)
- Ultimate: Illocution+Validation (Vk)
- Antepenultimate: Format

```haskell
detectStress :: Text -> Stress
```

---

## Phase 2: V3 Parser (Elartkha)

### V3 Morphological Structure

V3 uses a similar but not identical slot structure to V4:

| Slot | Name | Content |
|------|------|---------|
| I | Cc | Concatenation status |
| II | Vv | Pattern + Stem + Designation |
| III | Cr | Root |
| IV | Vr | Function + Specification + Context |
| V | VxCs | Affixes |
| VI | Ca | Config + Extension + Affiliation + Perspective + Essence |
| VII | VnCn | Valence + Phase/Mood/Sanction |
| VIII | CsVx | More affixes |
| IX | Vc | Case |

### Key Differences from V4

1. **Designation** (2 values): Formal vs Informal
   - Moved from Ca to Slot II in V3
   - Replaced by "Effect" in V4

2. **Sanction** (9 values): Evidentiality-like
   - Factual, Propositional, Epistemological, etc.
   - Replaced by "Validation" in V4

3. **Pattern** (3 values): P1, P2, P3
   - Affects root interpretation
   - Replaced by "Specification" in V4

### V3 Implementation

```haskell
-- New module: Ithkuil.V3.Grammar
module Ithkuil.V3.Grammar where

data Designation = Formal | Informal

data Pattern = P1 | P2 | P3

data Sanction
  = SNC_PRP  -- Propositional
  | SNC_EPI  -- Epistemological
  | SNC_ALG  -- Allegorical
  | SNC_IPU  -- Imputative
  | SNC_RFU  -- Refutative
  | SNC_REB  -- Rebuttative
  | SNC_CJT  -- Conjectural (theoretical)
  | SNC_EXV  -- Expatiative
  | SNC_AXM  -- Axiomatic

-- V3 formative structure
data V3Formative = V3Formative
  { v3SlotII  :: (Pattern, Stem, Designation)
  , v3Root    :: Root
  , v3SlotIV  :: (Function, Specification, Context)
  , v3Ca      :: V3CaComplex
  , v3Case    :: Case
  }
```

### Parsing "Elartkha"

```
E - l - a - r - t - kh - a
  V  C   V   C   C   C    V
```

Conjunct split: `e - l - a - rtkh - a`
- `e`: Slot II vowel (Pattern + Stem + Designation)
- `l`: Root consonant (-L- = language/speech)
- `a`: Slot IV vowel (Function + Spec + Context)
- `rtkh`: Ca complex (unusual cluster)
- `a`: Case vowel

---

## Phase 3: V2 Parser (ilaks)

### V2 Key Features

1. **Reduced Phonemic Inventory**: 30 consonants (from V1's 65)
2. **Tonal System**: Added tones for grammatical marking
3. **Simplified Morphology**: Fewer categories

### V2 Slot Structure

Similar to V3 but with tonal markers on vowels affecting meaning.

### Tonal System

| Tone | Marking | Function |
|------|---------|----------|
| High | acute accent | Various grammatical markers |
| Low | grave accent | Various grammatical markers |
| Rising | circumflex | Various grammatical markers |
| Falling | caron | Various grammatical markers |

### Parsing "ilaks"

```
i - l - a - k - s
V   C   V   C   C
```

- `i`: Slot II (with tone)
- `l`: Root (-L-)
- `a`: Slot IV (with tone - marked as `a` in ilaks)
- `ks`: Ca/Case complex

```haskell
-- New module: Ithkuil.V2.Grammar
module Ithkuil.V2.Grammar where

data Tone = High | Low | Rising | Falling | Neutral

data V2Vowel = V2Vowel
  { v2Base :: Char
  , v2Tone :: Tone
  }

-- V2 formative with tonal information
data V2Formative = V2Formative
  { v2SlotII :: (V2Vowel, Stem, Designation)
  , v2Root   :: Root
  , v2SlotIV :: (V2Vowel, Function, Specification, Context)
  , v2Ca     :: V2CaComplex
  , v2Case   :: Case
  }
```

---

## Phase 4: V1 Parser (Itkuil)

### V1 Key Features

1. **Large Phonemic Inventory**: 65 consonants, 17 vowels
2. **Consonant Grades**: Systematic consonant mutations
3. **Complex Morphophonology**: Vocalic infixes with mutations

### V1 Etymology Analysis (from Wikipedia)

The word `itkuil` derives from root **-K-L-** (speech/voice/interpretation):

1. **-u- vocalic infix** -> `kul` = "a word" (Stem 2 Holistic)
2. **u -> ui mutation** -> `kuil` = "imaginary word" (Secondary Mode)
3. **Grade 8: k -> tk** -> `tkuil` = "vocabulary of imaginary words" (Composite Config)
4. **i- prefix** -> `itkuil` = "complete imaginary language" (Delimitive + Coalescent)

### Consonant Grade System

| Grade | Mutation | Example |
|-------|----------|---------|
| 1 | base | k |
| 2 | geminate | kk |
| 3 | aspirate | kh |
| 4 | glottalize | k' |
| 5 | palatalize | ky |
| 6 | velarize | kw |
| 7 | fricativize | x |
| 8 | add dental | tk |

### V1 Implementation

```haskell
-- New module: Ithkuil.V1.Grammar
module Ithkuil.V1.Grammar where

data ConsonantGrade = G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8

data V1Mode = Primary | Secondary

-- V1 uses biliteral roots (like Semitic)
data V1Root = V1Root
  { v1C1 :: Char  -- First radical
  , v1C2 :: Char  -- Second radical
  }

-- Grade mutations
applyGrade :: ConsonantGrade -> Char -> Text

-- Parse V1 formative
parseV1Formative :: Text -> Maybe V1Formative

data V1Formative = V1Formative
  { v1Pattern     :: Pattern        -- From vocalic infix
  , v1Stem        :: Stem           -- From vocalic infix position
  , v1Root        :: V1Root         -- Biliteral root
  , v1Configuration :: Configuration -- From consonant grade
  , v1Mode        :: V1Mode         -- From vowel mutation
  , v1Extension   :: Extension      -- From prefix
  , v1Affiliation :: Affiliation    -- From prefix
  }
```

### Parsing "Itkuil"

```
I - t - k - u - i - l
V   C   C   V   V   C
```

Analysis:
- `i-`: Prefix (Delimitive Extension + Coalescent Affiliation)
- `tk`: Consonant with Grade 8 mutation (base: k)
- `ui`: Vocalic infix (Secondary Mode)
- `l`: Second radical of root -K-L-

```haskell
parseItkuil :: Text -> V1Formative
parseItkuil "itkuil" = V1Formative
  { v1Pattern = Holistic
  , v1Stem = Stem2
  , v1Root = V1Root 'k' 'l'
  , v1Configuration = Composite
  , v1Mode = Secondary
  , v1Extension = Delimitive
  , v1Affiliation = Coalescent
  }
```

---

## Implementation Order

### Step 1: V4 Completion
1. Add complete Ca parsing tables
2. Add all 68 case vowel patterns
3. Add affix parsing
4. Add stress detection
5. Test with full Maleutrait analysis

### Step 2: V3 Module
1. Create `Ithkuil.V3.Grammar` module
2. Define V3-specific types (Designation, Sanction, Pattern)
3. Create V3 slot tables
4. Implement `parseV3Formative`
5. Test with Elartkha

### Step 3: V2 Module
1. Create `Ithkuil.V2.Grammar` module
2. Define tonal vowel system
3. Adapt V3 structures for V2
4. Implement `parseV2Formative`
5. Test with ilaks

### Step 4: V1 Module
1. Create `Ithkuil.V1.Grammar` module
2. Define consonant grade system
3. Define biliteral root system
4. Implement grade mutation functions
5. Implement `parseV1Formative`
6. Test with Itkuil

### Step 5: Unified Gloss Interface
1. Create `Ithkuil.Gloss` module
2. Detect version from phonological features
3. Route to appropriate parser
4. Generate unified gloss output

---

## File Structure

```
src/Ithkuil/
  Grammar.hs          # V4 grammar (current)
  Parse.hs            # V4 parser (current)
  Lexicon.hs          # Shared lexicon
  V3/
    Grammar.hs        # V3-specific types
    Parse.hs          # V3 parser
  V2/
    Grammar.hs        # V2-specific types
    Parse.hs          # V2 parser
  V1/
    Grammar.hs        # V1-specific types
    Parse.hs          # V1 parser
  Gloss.hs            # Unified glossing interface
```

---

## Testing Strategy

Each parser should be tested with:
1. The version's native name for "Ithkuil"
2. Sample sentences from official documentation
3. Edge cases (elided slots, unusual clusters)

```haskell
-- In test/Main.hs
spec :: Spec
spec = do
  describe "V4 Parser" $ do
    it "parses Maleutrait" $ ...

  describe "V3 Parser" $ do
    it "parses Elartkha" $ ...

  describe "V2 Parser" $ do
    it "parses ilaks" $ ...

  describe "V1 Parser" $ do
    it "parses Itkuil" $ ...
```

---

## Notes

- V3 documentation is most complete (archived at ithkuil.net)
- V1 and V2 documentation is partial (some via ithkuil.place mirror)
- IthkuilGloss (Kotlin reference) only handles V4
- Each version has unique phonotactic constraints affecting parsing
