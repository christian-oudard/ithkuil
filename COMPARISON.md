# Ithkuil Grammar Implementation Comparison

Three implementations of Ithkuil V4 grammar in Haskell, OCaml, and Lean 4.

## Lines of Code

| Language | Phonology | Grammar | Parse | Render | Total |
|----------|-----------|---------|-------|--------|-------|
| Haskell  | 111       | 441     | 112   | 208    | 872   |
| OCaml    | 94        | 238     | 77    | 121    | 530   |
| Lean 4   | 68        | 289     | 83    | 127    | 567   |

OCaml is most concise overall. Lean is concise for parsing/rendering.
Haskell is most verbose (includes detailed documentation comments).

## Type Expressiveness

### Sum Types (Variants/Enums)

**Haskell:**
```haskell
data Stem = S1 | S2 | S3 | S0
  deriving (Show, Eq, Ord, Enum, Bounded)
```
- Excellent: deriving gives you Show, Eq, Ord, Enum, Bounded for free
- Can enumerate all values with `[minBound..maxBound]`

**OCaml:**
```ocaml
type stem = S1 | S2 | S3 | S0
```
- Clean syntax
- No automatic deriving (need ppx_deriving for show, etc.)
- Pattern matching exhaustiveness checked

**Lean 4:**
```lean
inductive Stem where
  | s1 | s2 | s3 | s0
deriving Repr, DecidableEq, Inhabited
```
- Excellent: `deriving` similar to Haskell
- `Repr` for printing, `DecidableEq` for equality
- `Inhabited` provides default value

**Winner: Tie (Haskell ≈ Lean 4 > OCaml)**

### Product Types (Records)

**Haskell:**
```haskell
data Formative = Formative
  { fSlotI :: Maybe ConcatenationStatus
  , fSlotII :: SlotII
  ...
  }
```
- Prefixed field names needed to avoid conflicts
- Record syntax is somewhat clunky

**OCaml:**
```ocaml
type formative = {
  slot_i : concatenation_status option;
  slot_ii : slot_ii;
  ...
}
```
- Clean syntax
- Field names scoped to type (since OCaml 4.01)

**Lean 4:**
```lean
structure Formative where
  slotI : Option ConcatenationStatus
  slotII : SlotII
  ...
```
- Very clean syntax
- Field names properly scoped
- Automatic constructor and projections

**Winner: Lean 4 > OCaml > Haskell**

### Type Aliases

**Haskell:** `type SlotII = (Stem, Version)` - Simple but no new type
**OCaml:** `type slot_ii = stem * version` - Same
**Lean 4:** `abbrev SlotII := Stem × Version` - Same, but `×` is nicer

**Winner: Tie**

## Pattern Matching

All three have excellent exhaustive pattern matching:

**Haskell:**
```haskell
slotIIToVv :: SlotII -> Text
slotIIToVv (S1, PRC) = vowelForm 1 1
slotIIToVv (S1, CPT) = vowelForm 1 2
...
```

**OCaml:**
```ocaml
let slot_ii_to_vv : slot_ii -> string = function
  | (S1, PRC) -> vowel_form ~series:1 ~form:1
  | (S1, CPT) -> vowel_form ~series:1 ~form:2
  ...
```
- `function` keyword is elegant for single-argument pattern match

**Lean 4:**
```lean
def slotIIToVv : SlotII → String
  | (.s1, .prc) => vowelForm 1 1
  | (.s1, .cpt) => vowelForm 1 2
  ...
```
- Dot notation for constructors is concise
- Equations-style definitions are clean

**Winner: OCaml ≈ Lean 4 > Haskell** (OCaml's `function`, Lean's dot notation)

## Tooling

| Aspect | Haskell | OCaml | Lean 4 |
|--------|---------|-------|--------|
| Build system | cabal/stack | dune | lake |
| LSP support | haskell-language-server | ocaml-lsp | lean4 server |
| Compile speed | Slow | Fast | Medium |
| Error messages | Decent | Good | Excellent |
| IDE support | Good (VSCode, Emacs) | Good | Excellent (VSCode) |
| Package ecosystem | Large (Hackage) | Medium (opam) | Small but growing |

**Winner: Lean 4 > OCaml > Haskell** (for new projects)

## Unique Strengths

### Haskell
- Mature ecosystem with many libraries
- Lazy evaluation (useful for infinite structures)
- Type classes (polymorphism)
- Strong community for parsing (parsec, megaparsec, attoparsec)

### OCaml
- Fast compilation
- Predictable performance (strict)
- Good FFI for C interop
- Practical industrial use (Jane Street)

### Lean 4
- Dependent types (can prove properties about grammar)
- Excellent IDE experience
- Fast and can compile to C
- Growing community, modern design
- Could prove grammar invariants as theorems

## Recommendations for Ithkuil

### For Learning/Prototyping: **OCaml**
- Fastest iteration cycle
- Clean syntax
- Easy to understand

### For Production Quality: **Haskell**
- Best parsing libraries
- Largest ecosystem
- Type classes useful for serialization

### For Correctness Guarantees: **Lean 4**
- Could prove: "every formative has a valid phonological form"
- Could prove: "parsing is inverse of rendering"
- Most modern design

### For Your Existing Code: **Haskell**
- You already have mamkait in Haskell
- Can build on existing Data.Bimap approach
- Familiar patterns

## Build Instructions

### Haskell
```bash
cd impl/haskell
cabal build
```

### OCaml
```bash
cd impl/ocaml
dune build
```

### Lean 4
```bash
cd impl/lean
lake build
```

## Next Steps

1. **Add parsing**: Implement `Text -> Maybe Formative`
2. **Add rendering**: Implement `Formative -> Text`
3. **Add tests**: Property-based testing (QuickCheck/QCheck/Plausible)
4. **Add root dictionary**: Load from lexicon.pdf data
5. **Add affix dictionary**: Load from affixes.pdf data
