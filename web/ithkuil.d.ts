// The browser's view of the Ithkuil parser.
//
// This file is one half of a contract; code/api/types.go is the other,
// and code/api/dts_test.go fails when they disagree. Change a shape
// here and the Go suite tells you which struct no longer matches, which
// is the only way a hand-written declaration file stays true.
//
// Every call is synchronous and returns a JSON string in an envelope,
// never a thrown exception: a word the parser rejects is an ordinary
// answer in this domain and the caller wants it beside the word that
// produced it. Wrap the module in `parse` below rather than calling it
// raw, so the envelope is unwrapped in one place.

/** The failure arm of an envelope. */
export interface ApiError {
  /** The whole of what went wrong. Never a guess at a correction. */
  message: string;
}

export type Envelope<T> = { ok: T } | { error: ApiError };

/** One written chunk of a word, with the slot it fills. */
export interface Segment {
  /** Hyphen-decorated chunk, as the breakdown prints it. */
  chunk: string;
  /** Bare chunk, no hyphens. */
  raw: string;
  /** "Cr", "Vr", "Ca", "Vx1", "Cs1", "Vc", "Vv", and so on. */
  slot: string;
  encodes: string[];
  /** Every code in this chunk is at its grammatical default. */
  defaults: boolean;
  /** A placeholder for a slot the romanization elides. */
  elided: boolean;
  /** 1-based index for affix Cs/Vx pairs; 0 otherwise. */
  ordinal: number;
  /** Affix Cs cluster, for a lexicon lookup. */
  cluster: string;
  /** Affix degree 1-9, read off the paired Vx. */
  degree: number;
}

export interface GlossaryEntry {
  category: string;
  code: string;
  name: string;
  meaning: string;
}

/** A formative's lexical identity and the meaning it selects. */
export interface Headword {
  /** Root, stem and specification together, as `"m" / S1 / BSC`. */
  code: string;
  meaning: string;
}

/**
 * One piece of the gloss line. Concatenating every `text` in order
 * reproduces `gloss` exactly, so render the pieces, make the codes
 * clickable, and you are still showing what the glosser wrote. Both are
 * sent because joining is trivial and the gloss syntax is not: which
 * mark separates two slots and which binds a degree to its affix is
 * knowledge that stays in Go.
 */
export interface GlossToken {
  text: string;
  /**
   * How the piece is written, not what it resolves to. A "code" is
   * worth offering `note()` for, and most values have none, which is
   * normal rather than a gap.
   */
  kind: "code" | "root" | "degree" | "punct";
}

/**
 * One phonotactic fault: the section 2 rule a word breaks. `stage` and
 * `code` are what to branch on, `fix` is the sentence to show a reader.
 */
export interface Violation {
  stage: string;
  code: string;
  found?: string;
  fix: string;
}

/** One published sentence with Quijada's own English. */
export interface Example {
  section: string;
  ithkuil: string;
  gloss?: string;
  english: string;
}

/**
 * One minimal word per grammatical value, differing from a fixed
 * baseline in that value alone. "Show me a word that differs only in
 * essence" is this, and the answer is a Compare view.
 */
export interface Sample {
  category: string;
  abbrev: string;
  word: string;
  /**
   * The value changes no letters, or is its category's default so the
   * gloss shows nothing for it. Either way the sample is the baseline
   * untouched, which is how the language says it.
   */
  unwritten?: boolean;
  unmarked?: boolean;
}

/**
 * The digraph input method mid-word. `pending` is the tail one more
 * keystroke could still change, which a field shows dim: "t" is a
 * letter until a "," arrives and makes it "ţ".
 */
export interface Input {
  committed: string;
  pending: string;
  display: string;
}

/**
 * One word of a parsed text. A word that would not read carries
 * `error` and nothing else; show it in place, marked, with the reason.
 */
export interface Word {
  romanization: string;
  type?: string;
  gloss?: string;
  error?: string;
  segments?: Segment[];
  headword?: Headword;
  glossary?: GlossaryEntry[];
  /** `gloss` in pieces. Both are sent. */
  glossTokens?: GlossToken[];
  /**
   * The per-formative breakdown of a concatenation chain, in written
   * order, dependents first and the parent last. Present only when
   * there is more than one, and then `segments`, `glossary` and
   * `headword` above are empty: a chain has no single breakdown, and
   * flattening one loses which member each slot belongs to.
   */
  members?: Member[];
  /**
   * The section 2 rules the word breaks. Separate from `error`: a word
   * can parse and still be unpronounceable, since the Ca tables
   * generate a few clusters our reading of section 2 rejects.
   */
  violations?: Violation[];
}

/** One formative of a concatenation chain. */
export interface Member {
  /** The part it plays: "head", "Type1 dependent". */
  role?: string;
  word: string;
  segments?: Segment[];
  glossary?: GlossaryEntry[];
  headword?: Headword;
  /** False when only the shape could be read; `note` says why. */
  decoded: boolean;
  note?: string;
}

/**
 * A gloss expression built back into a word. `gloss` is the canonical
 * gloss of what was built, which is not always the expression asked
 * for: rendering is canonical, so an equivalent spelling comes back
 * normalized and the page can show that it did.
 */
export interface Composed {
  word: string;
  gloss: string;
}

export interface SlotRow {
  slot: string;
  a: Segment;
  b: Segment;
  differs: boolean;
}

export interface GlossRow {
  category: string;
  a: GlossaryEntry;
  b: GlossaryEntry;
}

export interface ComparePair {
  /** A chain member's part ("head", "Type1 dependent"), else empty. */
  role: string;
  slots: SlotRow[];
  gloss: GlossRow[];
}

/** A chain member with nothing on the other side to compare against. */
export interface Unpaired {
  word: string;
  role: string;
  owner: string;
}

export interface Comparison {
  a: string;
  b: string;
  pairs: ComparePair[];
  unpaired: Unpaired[];
}

/**
 * One value of the grammar inventory. The same rows are the reference a
 * learner browses and the options in a builder control, which is why
 * the builder needs no table of its own.
 */
export interface GrammarEntry {
  category: string;
  abbrev: string;
  name: string;
  form?: string;
  description?: string;
  /**
   * The fuller reading of the value, and how it lands in English. Both
   * arrive with the notes document and are empty until it is loaded;
   * 160 of the 294 values have them. A value with neither has nothing
   * surprising about it, which is not a gap to fill by invention.
   */
  explanation?: string;
  guidance?: string;
}

/**
 * An explanation belonging to no single value of a category: a
 * construction, a slot, an affix pattern, or a value read in a second
 * context (an illocution carried by a Vk affix rather than a slot).
 */
export interface Topic {
  key: string;
  category: string;
  name?: string;
  explanation?: string;
  guidance?: string;
}

/**
 * One place in a formative a builder offers controls for. `categories`
 * are named as `table()` names them, so a control's options are one
 * call away and the front end carries no mapping of its own: which
 * category is edited in which slot is a fact about the language.
 */
export interface Position {
  /** The section 3 slot number, "I" through "X". */
  slot: string;
  /** The slot label the segment breakdown prints. Absent for Slot X. */
  field?: string;
  name: string;
  categories?: string[];
  /** A slot whose reading depends on something outside it. */
  note?: string;
}

/** A lexicon root. `stems` is indexed by stem number, 0 through 3. */
export interface Root {
  cr: string;
  stems: string[];
  contential?: string;
  constitutive?: string;
  objective?: string[];
  completive?: string[];
  dynamic?: string;
}

export interface RootHit {
  /** Lower is better; 0 is a direct cluster match. */
  score: number;
  root: Root;
}

/** One affix and its nine degrees, read down as a gradient. */
export interface Affix {
  cs: string;
  abbrev: string;
  description: string;
  type: string;
  degrees: string[];
}

export interface SearchResult {
  grammar: GrammarEntry[];
  roots: RootHit[];
  affixes: Affix[];
}

/** An English headword read backwards into a lexical core. */
export interface Sense {
  cr: string;
  stem: string;
  gloss: string;
  word: string;
}

/** Both counts are zero before `load`, which is a legitimate state. */
export interface LexiconInfo {
  version: number;
  roots: number;
  affixes: number;
  explained: number;
  topics: number;
}

export interface Info {
  /**
   * Bumped when a shape changes in a way a page compiled against the
   * old one would misread. A page that finds a version it does not know
   * has a stale cached bundle and should say so.
   */
  apiVersion: number;
  lexicon: LexiconInfo;
}

/**
 * The module, as TinyGo installs it on globalThis. Every method returns
 * a JSON-encoded Envelope; use `parse` to unwrap.
 */
export interface Ithkuil {
  /**
   * What has been loaded, and `apiVersion`. Check that against the
   * version this page was written for: a mismatch is a stale cached
   * bundle, and saying so beats rendering a shape you misread.
   */
  info(): string;
  /**
   * Merge a lexicon document. Either of its "roots" and "affixes" keys
   * may be absent, so affixes (54 KB) and roots (260 KB) can be fetched
   * separately and the first shown as soon as it lands. Merging, not
   * replacing: a later call carrying only roots keeps the affixes.
   */
  load(json: string): string;
  /** ASCII digraphs are accepted, so no keyboard layout is needed. */
  parse(text: string): string;
  /**
   * Build a word from a gloss expression. `-` separates slots, `.`
   * joins category values inside one, `/` binds a degree or a case to a
   * head, `_` trails the affix Type, and `{Ca}` is an all-default Ca
   * that still marks that boundary. Affixes written before the Ca land
   * in Slot V. `ml`, `S2.CPT-ml-ERG`, `m-SYS/5_2-{Ca}-DCD/1_2`.
   */
  compose(expr: string): string;
  /** Fails on a word with no slot structure, such as an adjunct. */
  compare(a: string, b: string): string;
  /**
   * One query against the grammar inventory and the lexicon at once,
   * grammar first. Answers from the grammar alone before `load`.
   */
  search(query: string): string;
  /** English backwards into lexical cores. Needs roots loaded. */
  define(word: string): string;
  /** The grammar inventory's category names, for `table`. */
  categories(): string;
  /** One category's values, or the whole inventory when empty. */
  table(category?: string): string;
  /** One affix and its whole degree ladder. Needs affixes loaded. */
  affix(cs: string): string;
  /** The published corpus, with Quijada's own English. */
  examples(): string;
  /** One minimal word per grammatical value. */
  inventory(): string;
  /** Run the digraph input method over a field's whole contents. */
  input(ascii: string): string;
  /** The formative's slots in written order. The builder's frame. */
  positions(): string;
  /** Explanations belonging to no single value of a category. */
  topics(): string;
  /** The authored note for one code, which a glossary row links to. */
  note(abbrev: string): string;
  /** "t," becomes "ţ", "sq" becomes "š". */
  fromASCII(text: string): string;
}

declare global {
  // eslint-disable-next-line no-var
  var ithkuil: Ithkuil;
}

/** Unwrap an envelope, throwing the module's own message on failure. */
export function parse<T>(raw: string): T {
  const env = JSON.parse(raw) as Envelope<T>;
  if ("error" in env) throw new Error(env.error.message);
  return env.ok;
}
