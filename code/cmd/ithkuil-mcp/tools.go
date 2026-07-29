package main

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/modelcontextprotocol/go-sdk/mcp"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/dictionary"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/slots"
	"github.com/christian-oudard/ithkuil/tokenize"
	"github.com/christian-oudard/ithkuil/view"
)

// registerTools wires every tool to its handler. The set mirrors the
// ithkuil CLI subcommands one-for-one: parse, compose, search, define.
func (s *server) registerTools(srv *mcp.Server) {
	mcp.AddTool(srv, &mcp.Tool{
		Name: "parse",
		Description: "Tokenize, parse, and gloss every word in the given Ithkuil text. " +
			"Returns the romanization, type (Form/Ref/Bias/...), one-line gloss, slot segments, " +
			"and per-word phonotactic validation, so parsing a word is also how you " +
			"check that it is pronounceable Ithkuil. Text may be written in the ASCII " +
			"digraph notation (aa→ä, t,→ţ, sq→š, e/→é); it is converted before parsing. " +
			"By default (verbose=false) descriptions are omitted; use the search tool " +
			"for separate lookups. Set verbose=true to include inline category names, " +
			"meanings, and root definition. Example: text=\"Maţřëullait\".",
	}, s.parse)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "compare",
		Description: "Lay two Ithkuil words' slot breakdowns side by side and report " +
			"what differs: one row per slot with each side's written chunk and codes " +
			"and whether they disagree, then the glossary categories whose code " +
			"changed. Answers what one letter is doing without diffing two parse " +
			"results by hand. A concatenation chain is compared member by member from " +
			"the parent end. Either word may use the ASCII digraph notation. " +
			"Example: a=\"mar\u00e7at\", b=\"marcat\".",
	}, s.compare)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "compose",
		Description: "Build a Ithkuil formative from a gloss-style expression. " +
			"Each punctuation mark has one job: '-' separates slots, '.' joins category " +
			"values inside a slot (S2.CPT, DYN.OBJ, MSS.G.RPV), '/' binds a degree or a " +
			"case to a head (DEV/3, ACC/INS), '_' trails the affix Type (t/1_2), and ':' " +
			"tags a stacked Ca (Ca:MSS.G). The root is a lowercase consonant cluster (Cr), " +
			"or (ABBREV)/degree for a CsRoot, or (1m+2p) for a RefRoot. Affixes placed " +
			"before the Ca land in Slot V (applying to the stem alone); write '{Ca}' for an " +
			"all-default Ca that still needs to mark that boundary. The returned romanization is " +
			"canonical. By default (verbose=false) descriptions are omitted. Set " +
			"verbose=true for inline names and meanings. Examples: expression=\"ml\" → " +
			"\"mlala\"; \"S2.CPT-ml-ERG\" → \"wimlo\"; " +
			"\"S2.CPT-ml-DYN.OBJ-MSS.G-DEV/3-ERG\" → \"imlötrebo\"; \"(CTR)/1\" → \"ëilal\"; " +
			"\"m-SYS/5_2-{Ca}-DCD/1_2\" → \"maţřëullait\".",
	}, s.compose)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "search",
		Description: "Look a term up in the grammar inventory and in the root and affix " +
			"lexicons at once. Grammar hits (entries) come back before lexicon hits " +
			"(roots, affixes), since a short query is more often a grammatical " +
			"abbreviation than a root. With no query and no category, returns the list " +
			"of category names. With category, lists all entries in that category (Case, " +
			"Aspect, Bias, Mood, ...). With exact=true, the query must equal an " +
			"abbreviation. With form=true, treats the query as a written form (vowel or " +
			"consonant) and answers from the grammar only. limit caps lexicon hits per " +
			"kind (default 20). Example: query=\"ERG\"; category=\"Case\"; " +
			"query=\"ëu\", form=true.",
	}, s.search)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "define",
		Description: "Look an English word up as the Ithkuil lexical cores that name it: " +
			"a root plus the stem, version, and specification selecting that sense, " +
			"rendered as a bare thematic formative. Case and illocution belong to the " +
			"sentence, not to a dictionary entry, so they are absent. Coverage of " +
			"English is partial: this reads the lexicon's own glosses backwards. " +
			"Example: word=\"crisis\".",
	}, s.define)
}

// --------------------------------------------------------------------
// parse
// --------------------------------------------------------------------

type parseIn struct {
	Text    string `json:"text" jsonschema:"one or more Ithkuil words"`
	Verbose bool   `json:"verbose,omitempty" jsonschema:"include category names, meanings, and root definition (default false)"`
}

type segmentOut struct {
	Chunk   string   `json:"chunk"`             // hyphen-decorated romanization
	Raw     string   `json:"raw"`               // bare written chunk
	Slot    string   `json:"slot"`              // Cr, Vr, Ca, Vx₁, Cs₁, …
	Encodes []string `json:"encodes"`           // codes encoded
	Default bool     `json:"default,omitempty"` // all encoded codes are defaults
	Elided  bool     `json:"elided,omitempty"`  // placeholder for absent slot
}

type glossaryRow struct {
	Category string `json:"category"`
	Code     string `json:"code"`
	Name     string `json:"name,omitempty"`
	Meaning  string `json:"meaning,omitempty"`
}

type validationOut struct {
	Rule    string `json:"rule"`
	Cluster string `json:"cluster"`
	Reason  string `json:"reason"`
}

type rootHead struct {
	Code    string `json:"code"`              // "\"m\" / S1 / BSC"
	Meaning string `json:"meaning,omitempty"` // stem-selected lexicon entry
}

type parseWord struct {
	Romanization string          `json:"romanization"`
	Type         string          `json:"type"`
	Gloss        string          `json:"gloss"`
	Reason       string          `json:"reason,omitempty"` // why an unclassified word could not be read
	Root         *rootHead       `json:"root,omitempty"`
	Segments     []segmentOut    `json:"segments,omitempty"`
	Glossary     []glossaryRow   `json:"glossary,omitempty"`
	Valid        bool            `json:"valid"`
	Violations   []validationOut `json:"violations,omitempty"`
}

type parseOut struct {
	Words []parseWord `json:"words"`
}

func (s *server) parse(_ context.Context, _ *mcp.CallToolRequest, in parseIn) (*mcp.CallToolResult, parseOut, error) {
	text := strings.TrimSpace(in.Text)
	if text == "" {
		return nil, parseOut{}, fmt.Errorf("text is required")
	}
	text = phonology.FromASCII(text)
	results := tokenize.Tokenize(text)
	span := tokenize.Words(results)
	glosser := gloss.Glosser{Lex: s.lex}

	out := make([]parseWord, len(results))
	for i, r := range results {
		w := parseWord{Romanization: r.Romanization}
		if r.Err != nil {
			// The shape split survives even when the grammatical
			// reading fails, so both the reason and the split are
			// available; without them the caller gets no way to tell
			// an unreadable word from an unsupported one.
			w.Type = "?"
			w.Gloss = "?" + r.Romanization
			w.Reason = view.UnknownReason(r.Romanization)
			if layout, err := slots.Parse(r.Romanization); err == nil {
				for _, sg := range view.LayoutSegments(layout) {
					w.Segments = append(w.Segments, segmentOut{
						Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
					})
				}
			}
			out[i] = w
			continue
		}
		w.Type = view.Type(r.Word)
		w.Gloss = glosser.Word(r.Word, span, i)
		switch tt := r.Word.(type) {
		case g.Formative:
			head := view.Headword(tt, s.lex)
			if head.Code != "" {
				r := &rootHead{Code: head.Code}
				if in.Verbose {
					r.Meaning = head.Meaning
				}
				w.Root = r
			}
			segs := view.Segments(r.Romanization, tt, s.lex)
			for _, sg := range segs {
				w.Segments = append(w.Segments, segmentOut{
					Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
					Encodes: sg.Encodes, Default: sg.Defaults, Elided: sg.Elided,
				})
			}
			if in.Verbose {
				for _, ge := range view.Glossary(r.Romanization, tt, segs, s.lex) {
					w.Glossary = append(w.Glossary, glossaryRow{
						Category: ge.Category, Code: ge.Code,
						Name: ge.Name, Meaning: ge.Meaning,
					})
				}
			}
		case g.ModularAdjunct:
			var marksMood *bool
			if verbal, found := tokenize.ModularIsVerbal(span, i); found {
				marksMood = &verbal
			}
			segs := view.SegmentsModular(r.Romanization, tt, marksMood)
			for _, sg := range segs {
				w.Segments = append(w.Segments, segmentOut{
					Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
					Encodes: sg.Encodes, Default: sg.Defaults, Elided: sg.Elided,
				})
			}
			if in.Verbose {
				for _, ge := range view.GlossaryModular(segs) {
					w.Glossary = append(w.Glossary, glossaryRow{
						Category: ge.Category, Code: ge.Code,
						Name: ge.Name, Meaning: ge.Meaning,
					})
				}
			}
		}
		var ill phonology.Illegal
		err := phonology.CheckText(r.Romanization)
		w.Valid = err == nil
		if errors.As(err, &ill) {
			for _, v := range ill.Violations {
				w.Violations = append(w.Violations, validationOut{
					Rule: v.Rule, Cluster: v.Cluster, Reason: v.Reason,
				})
			}
		}
		out[i] = w
	}
	return nil, parseOut{Words: out}, nil
}

// --------------------------------------------------------------------
// compare
// --------------------------------------------------------------------

type compareIn struct {
	A string `json:"a" jsonschema:"the first word"`
	B string `json:"b" jsonschema:"the second word"`
}

// slotRowOut is one slot lined up across both words. A side with no
// such slot has an empty chunk and no codes.
type slotRowOut struct {
	Slot     string   `json:"slot"`
	AChunk   string   `json:"a_chunk,omitempty"`
	AEncodes []string `json:"a_encodes,omitempty"`
	BChunk   string   `json:"b_chunk,omitempty"`
	BEncodes []string `json:"b_encodes,omitempty"`
	Differs  bool     `json:"differs,omitempty"`
}

type glossDiffOut struct {
	Category string `json:"category"`
	ACode    string `json:"a_code,omitempty"`
	AName    string `json:"a_name,omitempty"`
	BCode    string `json:"b_code,omitempty"`
	BName    string `json:"b_name,omitempty"`
}

// comparePairOut is one pair of formatives compared. Words that are not
// chains give exactly one pair.
type comparePairOut struct {
	A           string         `json:"a"` // header: romanization, plus chain role
	B           string         `json:"b"`
	Slots       []slotRowOut   `json:"slots"`
	Differences []glossDiffOut `json:"differences,omitempty"`
	RootDiffers bool           `json:"root_differs,omitempty"`
	ARoot       *rootHead      `json:"a_root,omitempty"`
	BRoot       *rootHead      `json:"b_root,omitempty"`
	ANote       string         `json:"a_note,omitempty"` // why it would not decode
	BNote       string         `json:"b_note,omitempty"`
	Identical   bool           `json:"identical,omitempty"`
}

// unpairedOut is a chain member the other word had no counterpart for.
type unpairedOut struct {
	Word  string `json:"word"`
	Role  string `json:"role,omitempty"`
	Owner string `json:"owner"`
}

type compareOut struct {
	A        string           `json:"a"`
	B        string           `json:"b"`
	Pairs    []comparePairOut `json:"pairs"`
	Unpaired []unpairedOut    `json:"unpaired,omitempty"`
}

func (s *server) compare(_ context.Context, _ *mcp.CallToolRequest, in compareIn) (*mcp.CallToolResult, compareOut, error) {
	a, err := compareSide(in.A, s.lex)
	if err != nil {
		return nil, compareOut{}, err
	}
	b, err := compareSide(in.B, s.lex)
	if err != nil {
		return nil, compareOut{}, err
	}

	pairs, extra := view.PairSides(a, b)
	out := compareOut{A: a.Word, B: b.Word}
	for _, p := range pairs {
		pair := comparePairOut{
			A:           p.A.Header(),
			B:           p.B.Header(),
			RootDiffers: view.RootDiffers(p.A, p.B),
			ANote:       p.A.Note,
			BNote:       p.B.Note,
		}
		changed := false
		for _, r := range view.SlotDiff(p.A, p.B) {
			changed = changed || r.Differs
			pair.Slots = append(pair.Slots, slotRowOut{
				Slot:     r.Slot,
				AChunk:   r.A.Chunk,
				AEncodes: r.A.Encodes,
				BChunk:   r.B.Chunk,
				BEncodes: r.B.Encodes,
				Differs:  r.Differs,
			})
		}
		for _, d := range view.GlossDiff(p.A, p.B) {
			pair.Differences = append(pair.Differences, glossDiffOut{
				Category: d.Category,
				ACode:    d.A.Code, AName: d.A.Name,
				BCode: d.B.Code, BName: d.B.Name,
			})
		}
		if pair.RootDiffers {
			pair.ARoot = &rootHead{Code: p.A.Head.Code, Meaning: p.A.Head.Meaning}
			pair.BRoot = &rootHead{Code: p.B.Head.Code, Meaning: p.B.Head.Meaning}
		}
		pair.Identical = p.A.Decoded && p.B.Decoded && !changed && !pair.RootDiffers && len(pair.Differences) == 0
		out.Pairs = append(out.Pairs, pair)
	}
	for _, e := range extra {
		out.Unpaired = append(out.Unpaired, unpairedOut{
			Word: e.Block.Word, Role: e.Block.Role, Owner: e.Owner,
		})
	}
	return nil, out, nil
}

// compareSide validates one word and breaks it down. An unpronounceable
// word is an error here, naming the rule it breaks, the same refusal
// the CLI makes before it compares anything.
func compareSide(word string, lex *lexicon.Lexicon) (view.Side, error) {
	word = phonology.FromASCII(strings.TrimSpace(word))
	if word == "" {
		return view.Side{}, fmt.Errorf("both a and b are required")
	}
	if err := phonology.CheckText(word); err != nil {
		return view.Side{}, fmt.Errorf("%s is not pronounceable Ithkuil: %w", word, err)
	}
	return view.BuildSide(word, lex)
}

// --------------------------------------------------------------------
// compose
// --------------------------------------------------------------------

type composeIn struct {
	Expression string `json:"expression" jsonschema:"gloss-style compose expression; '-' separates slots, '.' joins category values in a slot, '/' binds a degree or case to a head; affixes before the Ca land in Slot V, with '{Ca}' marking an all-default Ca; bare cluster like 'ml' or full 'S2.CPT-ml-DYN.OBJ-MSS.G-DEV/3-ERG'"`
	Verbose    bool   `json:"verbose,omitempty" jsonschema:"include category names, meanings, and root definition (default false)"`
}

type composeOut struct {
	Romanization string        `json:"romanization"`
	Gloss        string        `json:"gloss"`
	Root         *rootHead     `json:"root,omitempty"`
	Segments     []segmentOut  `json:"segments,omitempty"`
	Glossary     []glossaryRow `json:"glossary,omitempty"`
}

func (s *server) compose(_ context.Context, _ *mcp.CallToolRequest, in composeIn) (*mcp.CallToolResult, composeOut, error) {
	expr := strings.TrimSpace(in.Expression)
	if expr == "" {
		return nil, composeOut{}, fmt.Errorf("expression is required")
	}
	var affixes map[string]lexicon.AffixEntry
	if s.lex != nil {
		affixes = s.lex.Affixes
	}
	f, err := compose.Formative(expr, affixes)
	if err != nil {
		return nil, composeOut{}, err
	}
	rom := render.Formative(f)
	glosser := gloss.Glosser{Lex: s.lex}
	segs := view.Segments(rom, f, s.lex)
	head := view.Headword(f, s.lex)
	out := composeOut{
		Romanization: rom,
		Gloss:        glosser.Formative(f),
	}
	if head.Code != "" {
		r := &rootHead{Code: head.Code}
		if in.Verbose {
			r.Meaning = head.Meaning
		}
		out.Root = r
	}
	for _, sg := range segs {
		out.Segments = append(out.Segments, segmentOut{
			Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
			Encodes: sg.Encodes, Default: sg.Defaults, Elided: sg.Elided,
		})
	}
	if in.Verbose {
		for _, ge := range view.Glossary(rom, f, segs, s.lex) {
			out.Glossary = append(out.Glossary, glossaryRow{
				Category: ge.Category, Code: ge.Code,
				Name: ge.Name, Meaning: ge.Meaning,
			})
		}
	}
	return nil, out, nil
}

// --------------------------------------------------------------------
// search
// --------------------------------------------------------------------

type searchIn struct {
	Query    string `json:"query,omitempty" jsonschema:"abbreviation, category, written form, or meaning substring"`
	Category string `json:"category,omitempty" jsonschema:"restrict grammar hits to one category (Case, Aspect, Bias, ...)"`
	Exact    bool   `json:"exact,omitempty" jsonschema:"if true, query must equal an abbreviation exactly"`
	Form     bool   `json:"form,omitempty" jsonschema:"if true, treat query as a written form (vowel or consonant); grammar only"`
	Limit    int    `json:"limit,omitempty" jsonschema:"maximum lexicon hits per kind (default 20)"`
}

type grammarEntryOut struct {
	Category    string `json:"category"`
	Abbrev      string `json:"abbrev"`
	Name        string `json:"name,omitempty"`
	Form        string `json:"form,omitempty"`
	Meaning     string `json:"meaning,omitempty"`
	Description string `json:"description,omitempty"` // Bias expression text only
}

type rootHitOut struct {
	Cr           string   `json:"cr"`
	Stem0        string   `json:"stem0,omitempty"`
	Stem1        string   `json:"stem1,omitempty"`
	Stem2        string   `json:"stem2,omitempty"`
	Stem3        string   `json:"stem3,omitempty"`
	Contential   string   `json:"contential,omitempty"`
	Constitutive string   `json:"constitutive,omitempty"`
	Objective    []string `json:"objective,omitempty"`
	Completive   []string `json:"completive,omitempty"`
	Dynamic      string   `json:"dynamic,omitempty"`
	Wikidata     []string `json:"wikidata,omitempty"`
}

type affixHitOut struct {
	Cs          string   `json:"cs"`
	Abbrev      string   `json:"abbrev"`
	Description string   `json:"description"`
	Type        string   `json:"type"`
	Degrees     []string `json:"degrees"`
}

// searchOut lists grammar hits before lexicon hits, the order the CLI
// prints them in and the order a short query is most often meant in.
type searchOut struct {
	Categories []string          `json:"categories,omitempty"`
	Entries    []grammarEntryOut `json:"entries,omitempty"`
	Roots      []rootHitOut      `json:"roots,omitempty"`
	Affixes    []affixHitOut     `json:"affixes,omitempty"`
}

func (s *server) search(_ context.Context, _ *mcp.CallToolRequest, in searchIn) (*mcp.CallToolResult, searchOut, error) {
	query := strings.TrimSpace(in.Query)
	if query == "" && in.Category == "" {
		return nil, searchOut{Categories: compose.Categories()}, nil
	}
	if in.Form && query == "" {
		return nil, searchOut{}, fmt.Errorf("form=true requires a query")
	}

	var out searchOut
	if in.Form {
		hits := compose.LookupForm(query)
		if in.Category != "" {
			hits = filterEntriesByCategory(hits, in.Category)
		}
		out.Entries = toGrammarEntries(hits)
		// A written form is a grammar question; the lexicon has no
		// answer to what a vowel encodes.
		return nil, out, nil
	}
	out.Entries = toGrammarEntries(compose.Filter(in.Category, query, in.Exact))

	if query == "" {
		return nil, out, nil
	}
	if s.st == nil {
		return nil, out, fmt.Errorf("data store not available")
	}
	limit := in.Limit
	if limit <= 0 {
		limit = 20
	}
	roots, err := s.st.SearchRoots(query, limit)
	if err != nil {
		return nil, searchOut{}, fmt.Errorf("root search: %w", err)
	}
	for _, h := range roots {
		out.Roots = append(out.Roots, rootHitOut{
			Cr:           h.Cr,
			Stem0:        h.Stem0,
			Stem1:        h.Stem1,
			Stem2:        h.Stem2,
			Stem3:        h.Stem3,
			Contential:   h.Contential,
			Constitutive: h.Constitutive,
			Objective:    h.Objective,
			Completive:   h.Completive,
			Dynamic:      h.Dynamic,
			Wikidata:     h.Wikidata,
		})
	}
	affixes, err := s.st.SearchAffixes(query, limit)
	if err != nil {
		return nil, searchOut{}, fmt.Errorf("affix search: %w", err)
	}
	for _, a := range affixes {
		out.Affixes = append(out.Affixes, affixHitOut{
			Cs:          a.Cs,
			Abbrev:      a.Abbrev,
			Description: a.Description,
			Type:        a.Type,
			Degrees:     a.Degrees,
		})
	}
	return nil, out, nil
}

func toGrammarEntries(hits []compose.Entry) []grammarEntryOut {
	out := make([]grammarEntryOut, len(hits))
	for i, e := range hits {
		out[i] = grammarEntryOut{
			Category:    e.Category,
			Abbrev:      e.Abbrev,
			Name:        e.Name,
			Form:        e.Form,
			Meaning:     g.Meaning(e.Abbrev),
			Description: e.Description,
		}
	}
	return out
}

func filterEntriesByCategory(in []compose.Entry, cat string) []compose.Entry {
	allowed := compose.Filter(cat, "", false)
	want := make(map[string]struct{}, len(allowed))
	for _, e := range allowed {
		want[e.Category+"|"+e.Abbrev] = struct{}{}
	}
	out := make([]compose.Entry, 0, len(in))
	for _, e := range in {
		if _, ok := want[e.Category+"|"+e.Abbrev]; ok {
			out = append(out, e)
		}
	}
	return out
}

// --------------------------------------------------------------------
// define
// --------------------------------------------------------------------

type defineIn struct {
	Word  string `json:"word" jsonschema:"an English word to look up"`
	Limit int    `json:"limit,omitempty" jsonschema:"maximum senses returned (default 20)"`
}

// senseOut is one lexical core naming the English word: the bare
// thematic formative, its canonical gloss, and the lexicon cell the
// headword was read out of.
type senseOut struct {
	Romanization string `json:"romanization"`
	Gloss        string `json:"gloss"`
	Meaning      string `json:"meaning"`
}

type defineOut struct {
	Word   string     `json:"word"`
	Senses []senseOut `json:"senses,omitempty"`
	More   int        `json:"more,omitempty"` // senses past the limit
}

func (s *server) define(_ context.Context, _ *mcp.CallToolRequest, in defineIn) (*mcp.CallToolResult, defineOut, error) {
	word := strings.TrimSpace(in.Word)
	if word == "" {
		return nil, defineOut{}, fmt.Errorf("word is required")
	}
	if s.lex == nil {
		return nil, defineOut{}, fmt.Errorf("lexicon not available")
	}
	limit := in.Limit
	if limit <= 0 {
		limit = 20
	}
	senses := dictionary.Build(s.lex.Roots).Lookup(word)
	out := defineOut{Word: word}
	glosser := &gloss.Glosser{Canonical: true}
	for i, sense := range senses {
		if i == limit {
			out.More = len(senses) - limit
			break
		}
		f := sense.Formative()
		out.Senses = append(out.Senses, senseOut{
			Romanization: render.Formative(f),
			Gloss:        glosser.Formative(f),
			Meaning:      sense.Gloss,
		})
	}
	return nil, out, nil
}
