package api

import (
	"encoding/json"
	"errors"
	"fmt"
	"sort"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/dictionary"
	"github.com/christian-oudard/ithkuil/fault"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/inventory"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/roman"
	"github.com/christian-oudard/ithkuil/search"
	"github.com/christian-oudard/ithkuil/slots"
	"github.com/christian-oudard/ithkuil/view"
)

// ErrNoLexicon is returned by the calls that need meanings before any
// have been loaded. It is a distinct error because it is a sequencing
// problem in the page and not a problem with what the user typed.
var ErrNoLexicon = errors.New("no lexicon loaded")

// API holds the loaded lexicon and answers the calls the front end
// makes. The zero value is usable and answers everything that does not
// need meanings, which is the whole parser.
type API struct {
	lex   *lexicon.Lexicon
	index dictionary.Index
	// notes is keyed by abbreviation, which is unique across the whole
	// grammar table, so a code needs no category to find its note.
	notes  map[string]note
	topics []Topic
	// lexSearch answers the lexicon half of a search. Nil means the
	// in-memory scan, which is all a browser can have.
	lexSearch LexiconSearch
}

// note is the authored half of a grammar value: what it means at more
// length than the one-line description, and how it lands in English.
type note struct{ explanation, guidance string }

// LexiconSearch is how the lexicon half of a search is answered. There
// are two engines and the difference is not cosmetic: SQLite's
// full-text index ranks by word, so "cat" does not answer with
// "indicate", while the in-memory scan matches a substring anywhere.
// Measured over the lexicon they disagree on 13 of 20 hits for "water"
// and 16 of 20 for "cat".
//
// A browser has only the in-memory scan, because the driver has no
// js/wasm build. The CLI and the MCP server have the index and should
// keep it, so this is injected rather than chosen here: deduplicating
// the two front ends onto this package must not quietly downgrade what
// they answer with.
type LexiconSearch interface {
	SearchRoots(query string, limit int) ([]lexicon.RootEntry, error)
	SearchAffixes(query string, limit int) ([]lexicon.AffixEntry, error)
}

// SetLexicon installs an already-built lexicon, for a caller that read
// it from the store rather than from the JSON a browser fetches.
func (a *API) SetLexicon(lex *lexicon.Lexicon) {
	a.lex = lex
	if lex != nil {
		a.index = dictionary.Build(lex.Roots)
	}
}

// SetNotes installs the authored explanations, for a caller reading
// them from the store rather than from the JSON a browser fetches.
// Without it a store-backed caller gets the terse description and none
// of the writing that says how a value lands in English.
func (a *API) SetNotes(entries []GrammarEntry, topics []Topic) {
	for _, e := range entries {
		if e.Explanation == "" && e.Guidance == "" {
			continue
		}
		if a.notes == nil {
			a.notes = map[string]note{}
		}
		a.notes[e.Abbrev] = note{e.Explanation, e.Guidance}
	}
	a.topics = append(a.topics, topics...)
}

// SetLexiconSearch replaces the in-memory scan. Pass store.Searcher(st)
// to get the full-text index.
func (a *API) SetLexiconSearch(s LexiconSearch) { a.lexSearch = s }

// New returns an API with no lexicon. Parse, Compose, Compare,
// Categories, Table and FromASCII all work in that state; Define and
// Affix do not, and Search answers from the grammar inventory alone.
func New() *API { return &API{} }

// Load merges a lexicon document into what is already loaded. The
// document is the same JSON the store is built from, and either of its
// "roots" and "affixes" keys may be absent, so a page can fetch the
// 54 KB of affixes and the 260 KB of roots separately and show meanings
// for the first as soon as it lands.
//
// Merging rather than replacing is what makes that work. A second call
// carrying only roots must not drop the affixes the first one brought.
func (a *API) Load(doc []byte) (Info, error) {
	var notes struct {
		Grammar []struct {
			Abbrev      string `json:"abbrev"`
			Explanation string `json:"explanation"`
			Guidance    string `json:"guidance"`
		} `json:"grammar"`
		Topics []Topic `json:"topics"`
	}
	if err := json.Unmarshal(doc, &notes); err != nil {
		return Info{}, err
	}
	for _, e := range notes.Grammar {
		if e.Explanation == "" && e.Guidance == "" {
			continue
		}
		if a.notes == nil {
			a.notes = map[string]note{}
		}
		a.notes[e.Abbrev] = note{e.Explanation, e.Guidance}
	}
	a.topics = append(a.topics, notes.Topics...)

	part, err := lexicon.Parse(doc)
	if err != nil {
		return Info{}, err
	}
	if a.lex == nil {
		a.lex = &lexicon.Lexicon{
			Roots:   map[string]lexicon.RootEntry{},
			Affixes: map[string]lexicon.AffixEntry{},
		}
	}
	for k, v := range part.Roots {
		a.lex.Roots[k] = v
	}
	for k, v := range part.Affixes {
		a.lex.Affixes[k] = v
	}
	if part.Version != 0 {
		a.lex.Version = part.Version
	}
	// The English index reads every root's every stem, so it is rebuilt
	// when roots arrive and not on every call that wants it.
	if len(part.Roots) > 0 {
		a.index = dictionary.Build(a.lex.Roots)
	}
	return a.Info(), nil
}

// Info reports the API version and what has been loaded.
func (a *API) Info() Info {
	var li LexiconInfo
	if a.lex != nil {
		li = LexiconInfo{
			Version: a.lex.Version,
			Roots:   len(a.lex.Roots),
			Affixes: len(a.lex.Affixes),
		}
	}
	li.Explained = len(a.notes)
	li.Topics = len(a.topics)
	return Info{APIVersion: APIVersion, Lexicon: li}
}

// Parse reads a span of romanization and reports one Word per written
// word. ASCII digraphs are accepted, so a page needs no keyboard
// layout. One word that will not read does not cost the rest of the
// span, which is why this goes through Tokenize and not ParseText.
func (a *API) Parse(text string) []Word {
	results := roman.Tokenize(phonology.FromASCII(text))
	// A modular adjunct's Cn reads as Mood or Case-Scope depending on
	// whether the formative it applies to is verbal, which is a fact
	// about its neighbours. The glosser gets the whole span for it.
	span := roman.Words(results)
	gl := &gloss.Glosser{Lex: a.lex}
	out := make([]Word, 0, len(results))
	for i, r := range results {
		w := Word{Romanization: r.Romanization}
		// A word that will not parse is often one that will not be
		// pronounced either, and the rule it breaks says more than the
		// parse failure does. It still has a shape, and showing it is
		// the point: it is what makes a bad word legible beside a good
		// one.
		w.Violations = violations(r.Romanization)
		a.breakdown(&w, r.Romanization)
		if r.Err != nil {
			w.Error = r.Err.Error()
			out = append(out, w)
			continue
		}
		w.Type = view.Type(r.Word)
		w.Gloss = gl.Word(r.Word, span, i)
		w.GlossTokens = glossTokens(w.Gloss)
		out = append(out, w)
	}
	return out
}

// Examples returns the published corpus with Quijada's own English.
func (a *API) Examples() []Example {
	in := corpus.Examples()
	out := make([]Example, len(in))
	for i, e := range in {
		out[i] = Example{
			Section: e.Section, Ithkuil: e.Ithkuil,
			Gloss: e.Gloss, English: e.English,
		}
	}
	return out
}

// Inventory returns one minimal word per grammatical value, each
// differing from a fixed baseline in that value alone. It is the guided
// tour of the grammar that already exists and is already checked.
func (a *API) Inventory() []Sample {
	in := inventory.Samples()
	out := make([]Sample, 0, len(in))
	for _, s := range in {
		text, err := roman.Word(s.Word)
		if err != nil {
			continue
		}
		out = append(out, Sample{
			Category: s.Category, Abbrev: s.Abbrev, Word: text,
			Unwritten: s.Unwritten, Unmarked: s.Unmarked,
		})
	}
	return out
}

// Input runs the digraph input method over a field's whole contents and
// reports what has resolved and what one more keystroke could still
// change. Stateless on purpose: a page holds the ASCII the reader typed
// and asks what it looks like, rather than mirroring a cursor.
func (a *API) Input(ascii string) Input {
	var st phonology.InputState
	for _, r := range ascii {
		st.Feed(r)
	}
	return Input{
		Committed: st.Committed(),
		Pending:   st.Pending(),
		Display:   st.Display(),
	}
}

// breakdown fills in the slot analysis. It goes through view.BuildSide
// rather than view.Segments so that every class with slot structure
// gets one and not just formatives: a modular adjunct has a Vn and a
// Cn to show, a concatenation chain has one breakdown per member, and a
// word that would not decode still has a shape split worth seeing
// beside the reason it failed.
//
// Referentials, biases, registers and carriers have no slot structure
// at all and BuildSide says so. That is not a failure of the word, so
// the error is dropped: the caller gets a type and a gloss, which is
// everything there is.
func (a *API) breakdown(w *Word, romanization string) {
	side, err := view.BuildSide(romanization, a.lex)
	if err != nil || len(side.Blocks) == 0 {
		return
	}
	if len(side.Blocks) > 1 {
		for _, b := range side.Blocks {
			w.Members = append(w.Members, member(b))
		}
		return
	}
	m := member(side.Blocks[0])
	w.Segments, w.Glossary, w.Headword = m.Segments, m.Glossary, m.Headword
}

func member(b view.Block) Member {
	m := Member{
		Role: b.Role, Word: b.Word, Decoded: b.Decoded, Note: b.Note,
		Segments: segments(b.Segs), Glossary: glossary(b.Gloss),
	}
	if b.Head.Code != "" {
		m.Headword = &Headword{Code: b.Head.Code, Meaning: b.Head.Meaning}
	}
	return m
}

// Compose builds a word from a gloss expression, which is the builder's
// other half: the controls edit the expression and read the letters
// back from here.
// stressless writes stress as a §4.8 parsing adjunct instead of a
// diacritic, for readers and fonts that cannot show one.
func (a *API) Compose(expr string, stressless bool) (Composed, error) {
	// ParseText, not ParseWord: a concatenation chain glosses as two
	// formatives with a space between them, and reading the whole
	// string as one word takes the space for part of a root.
	words, err := gloss.ParseText(expr, a.lex)
	if err != nil {
		return Composed{}, err
	}
	if len(words) != 1 {
		return Composed{}, fmt.Errorf("%q is %d words; compose builds one", expr, len(words))
	}
	w := words[0]
	var text string
	if stressless {
		text, err = roman.Stressless(g.Text{w})
	} else {
		text, err = roman.Word(w)
	}
	if err != nil {
		return Composed{}, err
	}
	// A word class can be real and still write nothing: NRR is the
	// unmarked register, so it has no adjunct. Returning the empty
	// string would read as a failure of the renderer.
	if text == "" {
		return Composed{}, fmt.Errorf("%s is unmarked and writes no word", expr)
	}
	return Composed{Word: text, Gloss: (&gloss.Glosser{Lex: a.lex}).Token(w)}, nil
}

// Compare lays two words' slot breakdowns against each other.
func (a *API) Compare(x, y string) (Comparison, error) {
	sa, err := view.BuildSide(phonology.FromASCII(x), a.lex)
	if err != nil {
		return Comparison{}, fmt.Errorf("%s: %w", x, err)
	}
	sb, err := view.BuildSide(phonology.FromASCII(y), a.lex)
	if err != nil {
		return Comparison{}, fmt.Errorf("%s: %w", y, err)
	}
	pairs, unpaired := view.PairSides(sa, sb)
	out := Comparison{
		A: sa.Word, B: sb.Word,
		Pairs: []ComparePair{}, Unpaired: []Unpaired{},
	}
	for _, p := range pairs {
		cp := ComparePair{
			Slots:       []SlotRow{},
			Gloss:       []GlossRow{},
			Role:        p.A.Role,
			RootDiffers: view.RootDiffers(p.A, p.B),
			ANote:       p.A.Note,
			BNote:       p.B.Note,
		}
		if cp.RootDiffers {
			cp.AHead = &Headword{Code: p.A.Head.Code, Meaning: p.A.Head.Meaning}
			cp.BHead = &Headword{Code: p.B.Head.Code, Meaning: p.B.Head.Meaning}
		}
		var changed bool
		for _, r := range view.SlotDiff(p.A, p.B) {
			changed = changed || r.Differs
			cp.Slots = append(cp.Slots, SlotRow{
				Slot:    r.Slot,
				A:       segment(r.A),
				B:       segment(r.B),
				Differs: r.Differs,
			})
		}
		for _, r := range view.GlossDiff(p.A, p.B) {
			cp.Gloss = append(cp.Gloss, GlossRow{
				Category: r.Category,
				A:        glossaryEntry(r.A),
				B:        glossaryEntry(r.B),
			})
		}
		cp.Identical = p.A.Decoded && p.B.Decoded && !changed &&
			!cp.RootDiffers && len(cp.Gloss) == 0
		out.Pairs = append(out.Pairs, cp)
	}
	for _, u := range unpaired {
		out.Unpaired = append(out.Unpaired, Unpaired{
			Word: u.Block.Word, Role: u.Block.Role, Owner: u.Owner,
		})
	}
	return out, nil
}

// Search answers one query against the grammar inventory and the
// lexicon at once. With no lexicon loaded it answers from the grammar
// alone rather than failing, because the grammar tables are compiled
// into this module and are always available.
func (a *API) Search(query string, opts SearchOptions) SearchResult {
	// The slices start empty rather than nil. A nil slice marshals as
	// null, and ithkuil.d.ts declares these as arrays, so a caller
	// doing .roots.length crashed on a query that found nothing, which
	// is the commonest query there is.
	out := SearchResult{
		Grammar: []GrammarEntry{},
		Roots:   []RootHit{},
		Affixes: []Affix{},
	}
	switch {
	case opts.Form:
		// A written form is a question about the grammar alone. Asking
		// the lexicon what root contains the letter "ëu" would bury the
		// answer under substring noise.
		hits := search.LookupForm(query)
		if opts.Category != "" {
			// "what does -a- write, among the Biases" is a fair
			// question and usually answered by nothing, which is the
			// useful answer.
			keep := map[string]bool{}
			for _, e := range search.Filter(opts.Category, "", false) {
				keep[e.Category+"/"+e.Abbrev] = true
			}
			var kept []search.Entry
			for _, e := range hits {
				if keep[e.Category+"/"+e.Abbrev] {
					kept = append(kept, e)
				}
			}
			hits = kept
		}
		out.Grammar = a.grammarEntries(hits)
		return out
	case opts.Category != "" || opts.Exact:
		out.Grammar = a.grammarEntries(search.Filter(opts.Category, query, opts.Exact))
	default:
		out.Grammar = a.grammarEntries(search.SearchGrammar(query))
	}
	// A category listing is a grammar request and has no lexicon half.
	if (a.lex == nil && a.lexSearch == nil) || query == "" || opts.Category != "" {
		return out
	}
	limit := opts.Limit
	if limit == 0 {
		limit = 20
	}
	if a.lexSearch != nil {
		// The index does its own ranking and limiting.
		roots, err := a.lexSearch.SearchRoots(query, limit)
		if err != nil {
			return out
		}
		for _, e := range roots {
			out.Roots = append(out.Roots, RootHit{Root: root(e)})
		}
		affixes, err := a.lexSearch.SearchAffixes(query, limit)
		if err != nil {
			return out
		}
		for _, e := range affixes {
			out.Affixes = append(out.Affixes, affix(e))
		}
		return out
	}
	for _, h := range search.SearchRoots(query, a.lex.Roots) {
		if limit >= 0 && len(out.Roots) >= limit {
			break
		}
		out.Roots = append(out.Roots, RootHit{Score: h.Score, Root: root(h.Entry)})
	}
	for _, e := range search.SearchAffixes(query, a.lex.Affixes) {
		if limit >= 0 && len(out.Affixes) >= limit {
			break
		}
		out.Affixes = append(out.Affixes, affix(e))
	}
	return out
}

// Affixes returns a window onto the affix table, ordered by cluster.
// Browsing needs this: a search box cannot find what you do not know
// the name of, and the affixes are the part learners complain about
// most. A limit of zero means all 528, which is 250 KB of JSON and fine
// to ask for once.
func (a *API) Affixes(offset, limit int) AffixPage {
	if a.lex == nil {
		return AffixPage{Items: []Affix{}}
	}
	keys := make([]string, 0, len(a.lex.Affixes))
	for k := range a.lex.Affixes {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	out := AffixPage{Total: len(keys), Offset: offset, Items: []Affix{}}
	for _, k := range window(keys, offset, limit) {
		out.Items = append(out.Items, affix(a.lex.Affixes[k]))
	}
	return out
}

// Roots returns a window onto the lexicon, ordered by cluster. Unlike
// the affixes there are 5,891 of them, so a caller browsing rather than
// searching wants a page at a time.
func (a *API) Roots(offset, limit int) RootPage {
	if a.lex == nil {
		return RootPage{Items: []Root{}}
	}
	keys := make([]string, 0, len(a.lex.Roots))
	for k := range a.lex.Roots {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	out := RootPage{Total: len(keys), Offset: offset, Items: []Root{}}
	for _, k := range window(keys, offset, limit) {
		out.Items = append(out.Items, root(a.lex.Roots[k]))
	}
	return out
}

// window clamps a slice to a page. An offset past the end is an empty
// page rather than an error: a caller stepping through a list should
// find the end, not a failure.
func window(keys []string, offset, limit int) []string {
	if offset < 0 {
		offset = 0
	}
	if offset >= len(keys) {
		return nil
	}
	keys = keys[offset:]
	if limit > 0 && limit < len(keys) {
		keys = keys[:limit]
	}
	return keys
}

// Define reads an English headword backwards into the lexical cores
// that name it.
// A limit of zero means 20; a negative one means no cap.
func (a *API) Define(word string, limit int) (Definition, error) {
	if a.index == nil {
		return Definition{}, ErrNoLexicon
	}
	if limit == 0 {
		limit = 20
	}
	senses := a.index.Lookup(word)
	out := Definition{Word: word}
	gl := &gloss.Glosser{}
	for i, s := range senses {
		if limit >= 0 && i == limit {
			out.More = len(senses) - limit
			break
		}
		f := s.Formative()
		out.Senses = append(out.Senses, Sense{
			Cr:      s.Cr,
			Stem:    s.Stem.String(),
			Meaning: s.Gloss,
			Word:    roman.Formative(f),
			Gloss:   gl.Formative(f),
		})
	}
	return out, nil
}

// Categories lists the grammar inventory's categories.
func (a *API) Categories() []string { return search.Categories() }

// Table returns one category's values, or the whole inventory when
// category is empty. This is what populates a builder control, so the
// builder carries no table of its own.
func (a *API) Table(category string) []GrammarEntry {
	if category == "" {
		return a.grammarEntries(search.Table)
	}
	return a.grammarEntries(search.Filter(category, "", false))
}

// Affix returns one affix's whole degree ladder.
func (a *API) Affix(cs string) (Affix, error) {
	if a.lex == nil {
		return Affix{}, ErrNoLexicon
	}
	e, ok := a.lex.Affixes[cs]
	if !ok {
		return Affix{}, fmt.Errorf("no affix is written %q", cs)
	}
	return affix(e), nil
}

// FromASCII turns the digraph notation into the orthography: "t," to
// "ţ", "sq" to "š". Nobody has these characters on a keyboard.
func (a *API) FromASCII(s string) string { return phonology.FromASCII(s) }

// Reply is the envelope every call answers in: {"ok":...} or
// {"error":{"message":...}}, so the front end has one shape to check
// and TypeScript has a union to discriminate on. Failures travel in the
// payload rather than as thrown exceptions, because in this domain a
// rejected word is an ordinary answer.
func Reply(v any, err error) string {
	var envelope map[string]any
	if err != nil {
		envelope = map[string]any{"error": Error{Message: err.Error()}}
	} else {
		envelope = map[string]any{"ok": v}
	}
	b, merr := json.Marshal(envelope)
	if merr != nil {
		// Marshalling a type declared in this package cannot fail on
		// data; reaching here means a type here is not encodable, which
		// is a defect and should read as one.
		b, _ = json.Marshal(map[string]any{
			"error": Error{Message: "api: cannot encode reply: " + merr.Error()},
		})
	}
	return string(b)
}

// Conversions from the internal types. They are separate functions
// rather than methods so the internal types stay ignorant of the wire.

func glossTokens(line string) []GlossToken {
	in := gloss.Tokens(line)
	out := make([]GlossToken, len(in))
	for i, t := range in {
		out[i] = GlossToken{Text: t.Text, Kind: string(t.Kind)}
	}
	return out
}

// violations reports the §2 rules a romanization breaks, or nothing.
func violations(text string) []Violation {
	var fs fault.Faults
	if !errors.As(phonology.CheckText(text), &fs) {
		return nil
	}
	out := make([]Violation, len(fs.List))
	for i, f := range fs.List {
		out[i] = Violation{
			Stage: f.Stage.String(), Code: f.Code,
			Found: f.Found, Fix: f.Fix,
		}
	}
	return out
}

func segment(s view.Segment) Segment {
	// A blank segment, standing for a slot the other side has and this
	// one does not, encodes nothing. Nil would marshal as null and
	// ithkuil.d.ts declares an array.
	encodes := s.Encodes
	if encodes == nil {
		encodes = []string{}
	}
	return Segment{
		Chunk: s.Chunk, Raw: s.Raw, Slot: s.Slot, Encodes: encodes,
		Defaults: s.Defaults, Elided: s.Elided, Ordinal: s.Ordinal,
		Cluster: s.Cluster, Degree: s.Degree,
	}
}

func segments(in []view.Segment) []Segment {
	out := make([]Segment, len(in))
	for i, s := range in {
		out[i] = segment(s)
	}
	return out
}

func glossaryEntry(e view.GlossaryEntry) GlossaryEntry {
	return GlossaryEntry{
		Category: e.Category, Code: e.Code, Name: e.Name, Meaning: e.Meaning,
	}
}

func glossary(in []view.GlossaryEntry) []GlossaryEntry {
	out := make([]GlossaryEntry, len(in))
	for i, e := range in {
		out[i] = glossaryEntry(e)
	}
	return out
}

func (a *API) grammarEntries(in []search.Entry) []GrammarEntry {
	out := make([]GrammarEntry, len(in))
	for i, e := range in {
		out[i] = GrammarEntry{
			Category: e.Category, Abbrev: e.Abbrev, Name: e.Name,
			Form: e.Form, Description: e.Description,
		}
		if n, ok := a.notes[e.Abbrev]; ok {
			out[i].Explanation, out[i].Guidance = n.explanation, n.guidance
		}
	}
	return out
}

// Positions returns the formative's slots in written order, with the
// grammar categories edited in each. This is the builder's frame.
func (a *API) Positions() []Position {
	in := slots.Positions()
	out := make([]Position, len(in))
	for i, p := range in {
		out[i] = Position{
			Slot: p.Slot, Field: p.Field, Name: p.Name,
			Categories: p.Categories, Note: p.Note,
		}
	}
	return out
}

// Topics returns every topic, for the reference view that lists them.
func (a *API) Topics() []Topic { return a.topics }

// Note returns the authored explanation for one code, which is what a
// glossary row links to. A code with no note is not a gap to fill by
// invention: only the values with something surprising about them have
// one.
func (a *API) Note(abbrev string) (GrammarEntry, error) {
	for _, e := range search.LookupGrammar(abbrev) {
		got := a.grammarEntries([]search.Entry{e})[0]
		return got, nil
	}
	for _, t := range a.topics {
		if t.Key == abbrev {
			return GrammarEntry{
				Category: t.Category, Abbrev: t.Key, Name: t.Name,
				Explanation: t.Explanation, Guidance: t.Guidance,
			}, nil
		}
	}
	return GrammarEntry{}, fmt.Errorf("no grammar value or topic is called %q", abbrev)
}

func root(e lexicon.RootEntry) Root {
	return Root{
		Cr:           e.Cr,
		Stems:        []string{e.Stem0, e.Stem1, e.Stem2, e.Stem3},
		Contential:   e.Contential,
		Constitutive: e.Constitutive,
		Objective:    e.Objective,
		Completive:   e.Completive,
		Dynamic:      e.Dynamic,
		Wikidata:     e.Wikidata,
	}
}

func affix(e lexicon.AffixEntry) Affix {
	return Affix{
		Cs: e.Cs, Abbrev: e.Abbrev, Description: e.Description,
		Type: e.Type, Degrees: e.Degrees,
	}
}
