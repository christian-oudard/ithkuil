# Ithkuil V4 Implementation Examples

Showing the same operations in all three languages.

## Creating a Minimal Formative

### Haskell

```haskell
import Ithkuil.Grammar
import Ithkuil.Render

-- Create formative for "ml" (language root)
let f = minimalFormative "ml"

-- Render to text
renderFormative f  -- => "amla"
```

### OCaml

```ocaml
open Grammar
open Render

(* Create formative for "ml" (language root) *)
let f = minimal_formative "ml"

(* Render to text *)
render_formative f  (* => "amla" *)
```

### Lean 4

```lean
import Ithkuil

open Ithkuil

-- Create formative for "ml" (language root)
let f := minimalFormative "ml"

-- Render to text
Render.renderFormative f  -- => "amla"
```

## Parsing a Simple Word

### Haskell

```haskell
import Ithkuil.Parse

parseSimpleWord "amal"
-- => Just ((S1, PRC), Root "m", (STA, BSC, EXS))

parseSlotII "e"   -- => Just (S2, PRC)
parseCase "ei"    -- => Just (Appositive GEN)
```

### OCaml

```ocaml
open Parse

parse_simple_word "amal"
(* => Some ((S1, PRC), Root "m", (STA, BSC, EXS)) *)

parse_slot_ii "e"   (* => Some (S2, PRC) *)
parse_case "ei"     (* => Some (Appositive GEN) *)
```

### Lean 4

```lean
import Ithkuil

open Ithkuil.Parse

parseSimpleWord "amal"
-- => some ((.s1, .prc), ⟨"m"⟩, (.sta, .bsc, .exs))

parseSlotII "e"   -- => some (.s2, .prc)
parseCase "ei"    -- => some (.appositive .gen)
```

## Working with Morphological Categories

### Changing Configuration

```haskell
-- Haskell: Set to Multiplex Similar Separate (MSS)
let f' = f { fSlotVI = (MSS, CSL, M_, DEL, NRM) }
renderSlotVI (fSlotVI f')  -- => "tl"
```

```ocaml
(* OCaml: Set to Multiplex Similar Separate (MSS) *)
let f' = { f with slot_vi = (MSS, CSL, M_, DEL, NRM) }
render_slot_vi f'.slot_vi  (* => "tl" *)
```

```lean
-- Lean: Set to Multiplex Similar Separate (MSS)
let f' := { f with slotVI := (.mss, .csl, .m, .del, .nrm) }
Render.renderSlotVI f'.slotVI  -- => "tl"
```

### All Configuration Values

| Abbrev | Haskell | OCaml | Lean | Meaning |
|--------|---------|-------|------|---------|
| UNI | `UNI` | `UNI` | `.uni` | Uniplex (single) |
| DSS | `DSS` | `DSS` | `.dss` | Duplex Similar Separate |
| MSS | `MSS` | `MSS` | `.mss` | Multiplex Similar Separate |
| MFF | `MFF` | `MFF` | `.mff` | Multiplex Fuzzy Fused |

### Case Patterns

```haskell
-- Haskell: Different cases
renderCase (Transrelative THM)  -- => "a" (Thematic)
renderCase (Transrelative ERG)  -- => "o" (Ergative)
renderCase (Appositive GEN)     -- => "ei" (Genitive)
renderCase (Associative PUR)    -- => "iä" (Purposive)
```

## Building Complex Formatives

### With Affixes

```haskell
-- Haskell: Add an affix
let affix = Affix { affixVowel = "a", affixConsonant = "sk", affixType = Type1Affix }
let f'' = f { fSlotVII = [affix] }
```

### With Different Version

```haskell
-- Completive version (action completed)
let f''' = f { fSlotII = (S1, CPT) }
slotIIToVv (fSlotII f''')  -- => "ä"
```

## Stress and Slot IX

The formative's stress determines how Slot IX is interpreted:

| Stress | Slot IX Interpretation |
|--------|------------------------|
| Penultimate (default) | Case (Vc) |
| Ultimate | Format or Illocution+Validation (Vk) |
| Antepenultimate | Special concatenated forms |

```haskell
-- Haskell: Framed verb (ultimate stress)
let verbal = f { fStress = Ultimate, fSlotIX = Right (IllocVal ASR OBS) }
```

## Vowel Form Table

The standard vowel sequence table (Series × Form):

```
       1    2    3    4    5    6    7    8    9
    ┌────────────────────────────────────────────
S1  │  a    ä    e    ë    i    ö    o    ü    u
S2  │  ai   au   ei   eu   ëi   ou   oi   iu   ui
S3  │  ia   iä   ie   ië   ëu   uö   uo   ue   ua
S4  │  ao   ae   ea   eo   eë   öe   oe   öa   oa
```

Access with:
- Haskell: `vowelForm 2 3` => "ei"
- OCaml: `vowel_form ~series:2 ~form:3` => "ei"
- Lean: `vowelForm 2 3` => "ei"
