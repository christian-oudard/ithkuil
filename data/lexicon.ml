(** Ithkuil V4 Lexicon Data
    Auto-generated from collaborative spreadsheet *)

(** Root entry *)
type root_entry = {
  cr : string;
  stem0 : string;
  stem1 : string;
  stem2 : string;
  stem3 : string;
}

(** Affix entry *)
type affix_entry = {
  cs : string;
  abbrev : string;
  description : string;
  affix_type : string;
  degrees : string list;
}

(** Statistics *)
let root_count = 4720
let affix_count = 527

(** Sample roots (full data loaded from JSON at runtime) *)
let sample_roots : root_entry list = [
  { cr = "b"; stem0 = "belief/doc"; stem1 = "believing"; stem2 = "faith/belief in doctrine"; stem3 = "dogma/unquestionable belief" };
  { cr = "bb"; stem0 = "even/level (parallel to reference plane)"; stem1 = "degree of evenness"; stem2 = "decrease in evenness"; stem3 = "increase in evenness" };
  { cr = "bbl"; stem0 = "sink fixture"; stem1 = "sink fixture"; stem2 = "use of a sink"; stem3 = "being deprived of a functioning sink" };
  { cr = "bbr"; stem0 = "washbasin"; stem1 = "washbasin"; stem2 = "use of a washbasin"; stem3 = "being deprived of a functioning washbasin" };
  { cr = "bbř"; stem0 = "bidet"; stem1 = "bidet"; stem2 = "use of a bidet"; stem3 = "being deprived of a functioning bidet" };
]

(** Sample affixes *)
let sample_affixes : affix_entry list = [
  { cs = "b"; abbrev = "DEV"; description = "Degree of Development"; affix_type = "D1"; degrees = ["reversal/undoing"; "reversal/undoing in large chunks"; "reversal little by little"; "moribund/stagnant"; "well-maintained/well-kept"; "one by one"; "little by little"; "by leaps and bounds"; "complete & irreversible achievement"] };
  { cs = "bc"; abbrev = "X14"; description = "Fourteen (used in the context of a number-base higher than ten)"; affix_type = "0"; degrees = ["14th in physical arrangement"; "14th in agreed-upon order"; "14th in hierarchical order"; "14th"; "w/14 instances"; "w/14 or more instances"; "w/14 parts/sections"; "w/14 nodes/connections/points"; "w/14 hierarchical levels/tiers"] };
  { cs = "bč"; abbrev = "KIN"; description = "Kinship Relation"; affix_type = "0"; degrees = ["having a full-blood relationship"; "half-related"; "paternally related"; "maternally related"; "double relation (kinship)"; "in-law relation"; "parallel-related (kinship)"; "cross-related (kinship)"; "legally adopted relation"] };
  { cs = "bç"; abbrev = "NCD"; description = "Non-Default Context + Non-Default Perspective"; affix_type = "B"; degrees = ["G/FNC"; "N/FNC"; "A/FNC"; "G/RPS"; "N/RPS"; "A/RPS"; "G/AMG"; "N/AMG"; "A/AMG"] };
  { cs = "bḑ"; abbrev = "PCM"; description = "Primary Construction Material"; affix_type = "0*"; degrees = ["made exclusively or primarily of wood"; "made exclusively or primarily of metal"; "made exclusively or primarily of ceramic/porcelain"; "made exclusively or primarily of rubber"; "made exclusively or primarily of glass"; "made exclusively or primarily of resin/natural secretion"; "made exclusively or primarily of plastic"; "made exclusively or primarily of artificial/synthetic material"; "made exclusively or primarily of stone"] };
]
