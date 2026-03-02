/-- Ithkuil V4 Lexicon Data
    Auto-generated from collaborative spreadsheet -/

namespace Ithkuil.Lexicon

/-- Root entry -/
structure RootEntry where
  cr : String
  stem0 : String
  stem1 : String
  stem2 : String
  stem3 : String
deriving Repr, Inhabited

/-- Affix entry -/
structure AffixEntry where
  cs : String
  abbrev : String
  description : String
  affixType : String
  degrees : List String
deriving Repr, Inhabited

/-- Statistics -/
def rootCount : Nat := 4720
def affixCount : Nat := 527

/-- Sample roots (full data loaded from JSON at runtime) -/
def sampleRoots : List RootEntry := [
  ⟨"b", "belief/doc", "believing", "faith/belief in doctrine", "dogma/unquestionable belief"⟩,
  ⟨"bb", "even/level (parallel to reference plane)", "degree of evenness", "decrease in evenness", "increase in evenness"⟩,
  ⟨"bbl", "sink fixture", "sink fixture", "use of a sink", "being deprived of a functioning sink"⟩,
  ⟨"bbr", "washbasin", "washbasin", "use of a washbasin", "being deprived of a functioning washbasin"⟩,
  ⟨"bbř", "bidet", "bidet", "use of a bidet", "being deprived of a functioning bidet"⟩,
]

/-- Sample affixes -/
def sampleAffixes : List AffixEntry := [
  ⟨"b", "DEV", "Degree of Development", "D1", ["reversal/undoing", "reversal/undoing in large chunks", "reversal little by little", "moribund/stagnant", "well-maintained/well-kept", "one by one", "little by little", "by leaps and bounds", "complete & irreversible achievement"]⟩,
  ⟨"bc", "X14", "Fourteen (used in the context of a number-base higher than ten)", "0", ["14th in physical arrangement", "14th in agreed-upon order", "14th in hierarchical order", "14th", "w/14 instances", "w/14 or more instances", "w/14 parts/sections", "w/14 nodes/connections/points", "w/14 hierarchical levels/tiers"]⟩,
  ⟨"bč", "KIN", "Kinship Relation", "0", ["having a full-blood relationship", "half-related", "paternally related", "maternally related", "double relation (kinship)", "in-law relation", "parallel-related (kinship)", "cross-related (kinship)", "legally adopted relation"]⟩,
  ⟨"bç", "NCD", "Non-Default Context + Non-Default Perspective", "B", ["G/FNC", "N/FNC", "A/FNC", "G/RPS", "N/RPS", "A/RPS", "G/AMG", "N/AMG", "A/AMG"]⟩,
  ⟨"bḑ", "PCM", "Primary Construction Material", "0*", ["made exclusively or primarily of wood", "made exclusively or primarily of metal", "made exclusively or primarily of ceramic/porcelain", "made exclusively or primarily of rubber", "made exclusively or primarily of glass", "made exclusively or primarily of resin/natural secretion", "made exclusively or primarily of plastic", "made exclusively or primarily of artificial/synthetic material", "made exclusively or primarily of stone"]⟩,
]

end Ithkuil.Lexicon
