// Package api is the contract between this Go and a browser: the calls
// the front end may make and the exact JSON each one answers with.
//
// It exists because the boundary was accidental before it was designed.
// cmd/ithkuil-wasm marshalled internal types straight to the wire, so a
// page read `Differs` and `Chunk` and `Encodes` — Go field names, in Go
// capitalization, chosen for Go callers. Renaming a field in view/ then
// silently broke the page, and the page's shape was discoverable only
// by running it. Every type below carries explicit json tags, so the
// wire format is a decision that changes when someone decides to change
// it.
//
// The types are close to the internal ones and deliberately not the
// same. They drop what a browser cannot use (Wikidata Q-IDs), flatten
// what it should not have to walk (a root's four stems are an array),
// and name things as TypeScript names them.
//
// This package builds on every platform. Only the thin adapter in
// cmd/ithkuil-wasm is js/wasm-only, which is what lets these types be
// tested by the normal suite and lets an HTTP server serve the same
// shapes later without a second contract.
package api

// APIVersion is bumped whenever a shape here changes in a way a page
// compiled against the old one would misread. A page that finds a
// version it does not know has a stale cached bundle and should say so
// rather than render nonsense.
const APIVersion = 1

// Error is the failure arm of every response. Message is the whole of
// what went wrong: the parser knows what it saw and what it expected,
// and that is what belongs here. Nothing guesses at a correction.
type Error struct {
	Message string `json:"message"`
}

// Segment is one written chunk of a word paired with the slot it fills
// and the codes it encodes.
type Segment struct {
	Chunk    string   `json:"chunk"`
	Raw      string   `json:"raw"`
	Slot     string   `json:"slot"`
	Encodes  []string `json:"encodes"`
	Defaults bool     `json:"defaults"`
	Elided   bool     `json:"elided"`
	Ordinal  int      `json:"ordinal"`
	Cluster  string   `json:"cluster"`
	Degree   int      `json:"degree"`
}

// GlossaryEntry is one row of the glossary under a parsed word.
type GlossaryEntry struct {
	Category string `json:"category"`
	Code     string `json:"code"`
	Name     string `json:"name"`
	Meaning  string `json:"meaning"`
}

// Headword is a formative's lexical identity: the root, stem and
// specification that together pick out the referent, and the meaning
// that triple selects.
type Headword struct {
	Code    string `json:"code"`
	Meaning string `json:"meaning"`
}

// GlossToken is one piece of the gloss line. Concatenating every Text
// in order reproduces Gloss exactly, so a page can render the pieces,
// make the codes clickable, and still be showing what the glosser
// wrote. Both are sent because joining is trivial and the gloss syntax
// is not: which mark separates two slots and which binds a degree to
// its affix is knowledge that stays in Go.
type GlossToken struct {
	Text string `json:"text"`
	// Kind is "code", "root", "degree" or "punct". It says how the
	// piece is written, not what it resolves to: a "code" is worth
	// offering a note for, and most values have none, which is normal.
	Kind string `json:"kind"`
}

// Violation is one phonotactic fault: the §2 rule a word breaks. The
// stage and code are what a caller branches on and the fix is the
// sentence it shows a reader, because a caller given only the prose
// would have to pattern-match English to act on it.
type Violation struct {
	Stage string `json:"stage"`
	Code  string `json:"code"`
	Found string `json:"found,omitempty"`
	Fix   string `json:"fix"`
}

// Example is one published sentence with Quijada's own English. The
// corpus is how people actually learn this, which is the one thing the
// prompt tool got right about its browsing surface.
type Example struct {
	Section string `json:"section"`
	Ithkuil string `json:"ithkuil"`
	Gloss   string `json:"gloss,omitempty"`
	English string `json:"english"`
}

// Sample is one minimal word per grammatical value, differing from a
// fixed baseline in that value alone. "Show me a word that differs only
// in essence" is this, and the answer is a Compare view.
type Sample struct {
	Category string `json:"category"`
	Abbrev   string `json:"abbrev"`
	Word     string `json:"word"`
	// Unwritten means the value changes no letters, and Unmarked means
	// it is its category's default, so the gloss shows nothing for it.
	// Either way the sample is the baseline untouched, which is how the
	// language says it and not a failure to build the word.
	Unwritten bool `json:"unwritten,omitempty"`
	Unmarked  bool `json:"unmarked,omitempty"`
}

// Input is the digraph input method mid-word. Pending is the tail that
// one more keystroke could still change, which a field shows dim: "t"
// is a letter until a "," arrives and makes it "ţ".
type Input struct {
	Committed string `json:"committed"`
	Pending   string `json:"pending"`
	Display   string `json:"display"`
}

// Word is one word of a parsed text. A word that would not read carries
// Error and nothing else, because a word the parser rejects is the
// interesting case here and not a fault: the page shows it in place,
// marked, with the reason beside it.
type Word struct {
	Romanization string          `json:"romanization"`
	Type         string          `json:"type,omitempty"`
	Gloss        string          `json:"gloss,omitempty"`
	Error        string          `json:"error,omitempty"`
	Segments     []Segment       `json:"segments,omitempty"`
	Headword     *Headword       `json:"headword,omitempty"`
	Glossary     []GlossaryEntry `json:"glossary,omitempty"`
	// GlossTokens is Gloss in pieces. Both are sent.
	GlossTokens []GlossToken `json:"glossTokens,omitempty"`
	// Members is the per-formative breakdown of a concatenation chain,
	// in written order, dependents first and the parent last (§3.1.7).
	// Present only when there is more than one, and then Segments,
	// Glossary and Headword above are empty: a chain has no single
	// breakdown, and flattening one would lose which member each slot
	// belongs to.
	Members []Member `json:"members,omitempty"`
	// Violations are the §2 rules the word breaks. A word can parse and
	// still be unpronounceable, so this is separate from Error: the Ca
	// tables generate a few clusters our reading of §2 rejects, and a
	// parser that refused them could not round-trip its own output.
	Violations []Violation `json:"violations,omitempty"`
}

// Member is one formative of a concatenation chain.
type Member struct {
	// Role names the part it plays, "head" or "Type1 dependent".
	Role     string          `json:"role,omitempty"`
	Word     string          `json:"word"`
	Segments []Segment       `json:"segments,omitempty"`
	Glossary []GlossaryEntry `json:"glossary,omitempty"`
	Headword *Headword       `json:"headword,omitempty"`
	// Decoded is false when only the shape could be read. Note carries
	// the decoder's complaint in that case.
	Decoded bool   `json:"decoded"`
	Note    string `json:"note,omitempty"`
}

// Composed is a gloss expression built back into a word. Gloss is the
// canonical gloss of what was actually built, which is not always the
// expression that was asked for: render is canonical, so a
// non-canonical spelling of the same grammar comes back normalized, and
// the page can show that it did.
type Composed struct {
	Word  string `json:"word"`
	Gloss string `json:"gloss"`
}

// SlotRow is one row of a slot-by-slot comparison, with an empty
// segment where that side has no such slot.
type SlotRow struct {
	Slot    string  `json:"slot"`
	A       Segment `json:"a"`
	B       Segment `json:"b"`
	Differs bool    `json:"differs"`
}

// GlossRow is one glossary category whose code changed between the two
// words. A zero-valued side means the category is absent there.
type GlossRow struct {
	Category string        `json:"category"`
	A        GlossaryEntry `json:"a"`
	B        GlossaryEntry `json:"b"`
}

// ComparePair is two formatives lined up. Role names a chain member's
// part ("head", "Type1 dependent") and is empty for a standalone word.
type ComparePair struct {
	Role  string     `json:"role"`
	Slots []SlotRow  `json:"slots"`
	Gloss []GlossRow `json:"gloss"`
}

// Unpaired is a chain member with nothing on the other side to compare
// against, and which of the two words it came from.
type Unpaired struct {
	Word  string `json:"word"`
	Role  string `json:"role"`
	Owner string `json:"owner"`
}

// Comparison is the whole answer to comparing two words.
type Comparison struct {
	A        string        `json:"a"`
	B        string        `json:"b"`
	Pairs    []ComparePair `json:"pairs"`
	Unpaired []Unpaired    `json:"unpaired"`
}

// GrammarEntry is one value of the grammar inventory. It is both a row
// of the reference a learner browses and an option in a builder
// control, which is why the builder needs no table of its own.
type GrammarEntry struct {
	Category    string `json:"category"`
	Abbrev      string `json:"abbrev"`
	Name        string `json:"name"`
	Form        string `json:"form,omitempty"`
	Description string `json:"description,omitempty"`
	// Explanation is the fuller reading of the value, and Guidance says
	// how it lands in English. Both arrive with the notes document and
	// are empty until it is loaded; 160 of the 294 values have them,
	// and a value with nothing surprising about it has neither.
	Explanation string `json:"explanation,omitempty"`
	Guidance    string `json:"guidance,omitempty"`
}

// Topic is an explanation belonging to no single value of a category: a
// construction, a slot, an affix pattern, or a value read in a second
// context (an illocution as carried by a Vk affix rather than a slot).
// Keyed by its own name because there is no abbreviation to hang it on.
type Topic struct {
	Key         string `json:"key"`
	Category    string `json:"category"`
	Name        string `json:"name,omitempty"`
	Explanation string `json:"explanation,omitempty"`
	Guidance    string `json:"guidance,omitempty"`
}

// Root is a lexicon root. Stems is indexed by stem number, so Stems[0]
// is the stem-0 generic meaning and Stems[1..3] the specializations;
// the internal type spells them as four named fields, which is not a
// shape anything iterating over stems wants.
//
// The remaining fields are the cross-slot alternates, present on a
// small minority of roots and omitted when blank. Wikidata Q-IDs are
// dropped: they are for reconciling the lexicon against an external
// database, not for reading a word.
type Root struct {
	Cr           string   `json:"cr"`
	Stems        []string `json:"stems"`
	Contential   string   `json:"contential,omitempty"`
	Constitutive string   `json:"constitutive,omitempty"`
	Objective    []string `json:"objective,omitempty"`
	Completive   []string `json:"completive,omitempty"`
	Dynamic      string   `json:"dynamic,omitempty"`
}

// RootHit is a root returned by a search, with the relevance score the
// ordering used. Lower is better and zero is a direct cluster match.
type RootHit struct {
	Score int  `json:"score"`
	Root  Root `json:"root"`
}

// Affix is one affix and its nine degrees, which is the unit the affix
// tables are learned in: the degrees are a gradient, and reading them
// as a ladder shows that in a way nine separate lookups do not.
type Affix struct {
	Cs          string   `json:"cs"`
	Abbrev      string   `json:"abbrev"`
	Description string   `json:"description"`
	Type        string   `json:"type"`
	Degrees     []string `json:"degrees"`
}

// SearchResult is one query answered against the grammar inventory and
// the lexicon at once. Grammar hits come first because a three-letter
// query is almost always a category abbreviation.
type SearchResult struct {
	Grammar []GrammarEntry `json:"grammar"`
	Roots   []RootHit      `json:"roots"`
	Affixes []Affix        `json:"affixes"`
}

// Sense is one English headword's reading as a lexical core: which
// root, which stem, the source gloss it was read out of, and the
// minimal word that carries it.
type Sense struct {
	Cr    string `json:"cr"`
	Stem  string `json:"stem"`
	Gloss string `json:"gloss"`
	Word  string `json:"word"`
}

// Position is one place in a formative a builder offers controls for.
// The categories are named as Table names them, so a control's options
// are one table() call away, and the builder carries no mapping of its
// own: which category is edited in which slot is a fact about the
// language, and Ithkapp putting it in `:disabled` attributes is what
// this exists to avoid.
type Position struct {
	Slot       string   `json:"slot"`
	Field      string   `json:"field,omitempty"`
	Name       string   `json:"name"`
	Categories []string `json:"categories,omitempty"`
	// Note records a slot whose reading depends on something outside
	// it, which is the part a front end cannot infer from Categories.
	Note string `json:"note,omitempty"`
}

// LexiconInfo reports what has been loaded so far. Both counts are zero
// before Load runs, which is a legitimate state: parsing needs no
// lexicon, only meanings do.
type LexiconInfo struct {
	Version   uint16 `json:"version"`
	Roots     int    `json:"roots"`
	Affixes   int    `json:"affixes"`
	Explained int    `json:"explained"`
	Topics    int    `json:"topics"`
}

// Info is what the module reports about itself.
type Info struct {
	APIVersion int         `json:"apiVersion"`
	Lexicon    LexiconInfo `json:"lexicon"`
}
