package main

import (
	"context"
	"fmt"
	"strings"

	"github.com/modelcontextprotocol/go-sdk/mcp"

	"github.com/christian-oudard/ithkuil/compose"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/tokenize"
	"github.com/christian-oudard/ithkuil/validation"
	"github.com/christian-oudard/ithkuil/view"
)

// registerTools wires every tool to its handler.
func (s *server) registerTools(srv *mcp.Server) {
	mcp.AddTool(srv, &mcp.Tool{
		Name: "analyze",
		Description: "Tokenize, parse, and gloss every word in the given Ithkuil text. " +
			"Returns surface, type (Form/Ref/Bias/...), one-line gloss, slot segments, " +
			"and per-word phonotactic validation. By default (verbose=false) descriptions " +
			"are omitted; use grammar/lexicon tools for separate lookups. Set verbose=true " +
			"to include inline category names, meanings, and root definition. " +
			"Example: text=\"Malëuţřait\".",
	}, s.analyze)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "compose",
		Description: "Build a surface Ithkuil formative from a gloss-style expression. " +
			"Slots are separated by '-', sub-fields by '/' (S2/CPT, DYN/OBJ) or '.' (Ca: " +
			"MSS.G.RPV). The root is a lowercase consonant cluster (Cr), or (ABBREV)/degree " +
			"for a CsRoot, or (1m+2p) for a RefRoot. Affixes write Cs/degree or ABBREV/degree, " +
			"with an optional :2 or :3 type tag. The returned surface is canonical. By default " +
			"(verbose=false) descriptions are omitted. Set verbose=true for inline names and " +
			"meanings. Examples: expression=\"ml\" → \"wamla\"; \"S2/CPT-ml-ERG\" → \"wimlo\"; " +
			"\"S2/CPT-ml-DYN/OBJ-MSS.G-DEV/3-ERG\" → \"imlötrebo\"; \"(CTR)/1\" → \"ëilal\".",
	}, s.compose)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "grammar",
		Description: "Look up the grammar inventory. With no args, lists every category " +
			"name. With abbrevs, batch-resolves a list of abbreviations in one call " +
			"(e.g. abbrevs=[\"THM\",\"STA\",\"BSC\"]). With category, lists all entries " +
			"in that category (Case, Aspect, Bias, Mood, ...). With query, substring " +
			"matches against abbreviation / category / surface form / description. With " +
			"exact=true, exact abbrev match. With form=true, treats query as a surface " +
			"form (vowel or consonant). Example: category=\"Case\"; abbrevs=[\"ERG\",\"ABS\"].",
	}, s.grammar)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "lexicon",
		Description: "Substring search the root and/or affix lexicons. Pass kind=\"root\", " +
			"\"affix\", or \"both\" (default both). Returns ranked hits with surface " +
			"clusters and meaning text. Use queries=[...] to search multiple terms in one " +
			"call, deduplicating results by cluster. " +
			"Example: query=\"speak\"; queries=[\"ml\",\"ţř\"], kind=\"root\".",
	}, s.lexicon)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "validate",
		Description: "Run phonotactic validation per word. Returns valid=true when every " +
			"word passes; otherwise per-word violations with rule ID, offending cluster, " +
			"and reason. Example: text=\"tttest\" → rule 1.7 (triple consonant).",
	}, s.validate)
}

// --------------------------------------------------------------------
// analyze
// --------------------------------------------------------------------

type analyzeIn struct {
	Text    string `json:"text" jsonschema:"one or more Ithkuil words"`
	Verbose bool   `json:"verbose,omitempty" jsonschema:"include category names, meanings, and root definition (default false)"`
}

type segmentOut struct {
	Chunk   string   `json:"chunk"`             // hyphen-decorated surface
	Raw     string   `json:"raw"`               // bare surface chunk
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

type analyzeWord struct {
	Surface    string          `json:"surface"`
	Type       string          `json:"type"`
	Gloss      string          `json:"gloss"`
	Root       *rootHead       `json:"root,omitempty"`
	Segments   []segmentOut    `json:"segments,omitempty"`
	Glossary   []glossaryRow   `json:"glossary,omitempty"`
	Valid      bool            `json:"valid"`
	Violations []validationOut `json:"violations,omitempty"`
}

type analyzeOut struct {
	Words []analyzeWord `json:"words"`
}

func (s *server) analyze(_ context.Context, _ *mcp.CallToolRequest, in analyzeIn) (*mcp.CallToolResult, analyzeOut, error) {
	text := strings.TrimSpace(in.Text)
	if text == "" {
		return nil, analyzeOut{}, fmt.Errorf("text is required")
	}
	tokens := tokenize.Tokenize(text)
	glosser := gloss.Glosser{Lex: s.lex}

	out := make([]analyzeWord, len(tokens))
	for i, t := range tokens {
		w := analyzeWord{
			Surface: t.Surface(),
			Type:    view.Type(t),
			Gloss:   glosser.Token(t),
		}
		switch tt := t.(type) {
		case tokenize.FormativeWord:
			head := view.Headword(tt.Formative, s.lex)
			if head.Code != "" {
				r := &rootHead{Code: head.Code}
				if in.Verbose {
					r.Meaning = head.Meaning
				}
				w.Root = r
			}
			segs := view.Segments(tt.Text, tt.Formative, s.lex)
			for _, sg := range segs {
				w.Segments = append(w.Segments, segmentOut{
					Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
					Encodes: sg.Encodes, Default: sg.Defaults, Elided: sg.Elided,
				})
			}
			if in.Verbose {
				for _, ge := range view.Glossary(tt.Text, tt.Formative, segs, s.lex) {
					w.Glossary = append(w.Glossary, glossaryRow{
						Category: ge.Category, Code: ge.Code,
						Name: ge.Name, Meaning: ge.Meaning,
					})
				}
			}
		case tokenize.ModularWord:
			segs := view.SegmentsModular(tt.Text, tt.Modular, tt.MarksMood)
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
		res := validation.ValidateWord(t.Surface())
		w.Valid = res.Valid
		for _, e := range res.Errors {
			w.Violations = append(w.Violations, validationOut{
				Rule: e.Rule, Cluster: e.Cluster, Reason: e.Reason,
			})
		}
		out[i] = w
	}
	return nil, analyzeOut{Words: out}, nil
}

// --------------------------------------------------------------------
// compose
// --------------------------------------------------------------------

type composeIn struct {
	Expression string `json:"expression" jsonschema:"gloss-style compose expression; slots separated by '-', sub-fields by '/' or '.'; bare cluster like 'ml' or full 'S2/CPT-ml-DYN/OBJ-MSS.G-DEV/3-ERG'"`
	Verbose    bool   `json:"verbose,omitempty" jsonschema:"include category names, meanings, and root definition (default false)"`
}

type composeOut struct {
	Surface  string        `json:"surface"`
	Gloss    string        `json:"gloss"`
	Root     *rootHead     `json:"root,omitempty"`
	Segments []segmentOut  `json:"segments,omitempty"`
	Glossary []glossaryRow `json:"glossary,omitempty"`
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
	surface := render.Formative(f)
	glosser := gloss.Glosser{Lex: s.lex}
	segs := view.Segments(surface, f, s.lex)
	head := view.Headword(f, s.lex)
	out := composeOut{
		Surface: surface,
		Gloss:   glosser.Formative(f),
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
		for _, ge := range view.Glossary(surface, f, segs, s.lex) {
			out.Glossary = append(out.Glossary, glossaryRow{
				Category: ge.Category, Code: ge.Code,
				Name: ge.Name, Meaning: ge.Meaning,
			})
		}
	}
	return nil, out, nil
}

// --------------------------------------------------------------------
// grammar
// --------------------------------------------------------------------

type grammarIn struct {
	Abbrevs  []string `json:"abbrevs,omitempty" jsonschema:"batch list of abbreviations to resolve (e.g. [\"THM\",\"STA\",\"BSC\"])"`
	Query    string   `json:"query,omitempty" jsonschema:"abbreviation, category, form, or description substring"`
	Category string   `json:"category,omitempty" jsonschema:"restrict to one category (Case, Aspect, Bias, ...)"`
	Exact    bool     `json:"exact,omitempty" jsonschema:"if true, query must equal Abbrev exactly"`
	Form     bool     `json:"form,omitempty" jsonschema:"if true, treat query as a surface form (vowel or consonant)"`
}

type grammarEntryOut struct {
	Category    string `json:"category"`
	Abbrev      string `json:"abbrev"`
	Name        string `json:"name,omitempty"`
	Form        string `json:"form,omitempty"`
	Meaning     string `json:"meaning,omitempty"`
	Description string `json:"description,omitempty"` // Bias expression text only
}

type grammarOut struct {
	Categories []string          `json:"categories,omitempty"`
	Entries    []grammarEntryOut `json:"entries,omitempty"`
}

func (s *server) grammar(_ context.Context, _ *mcp.CallToolRequest, in grammarIn) (*mcp.CallToolResult, grammarOut, error) {
	// Batch abbreviation lookup.
	if len(in.Abbrevs) > 0 {
		var hits []compose.Entry
		for _, a := range in.Abbrevs {
			hits = append(hits, compose.Filter("", a, true)...)
		}
		return nil, grammarOut{Entries: toGrammarEntries(hits)}, nil
	}
	// No filters at all → return category list.
	if in.Query == "" && in.Category == "" && !in.Form {
		return nil, grammarOut{Categories: compose.Categories()}, nil
	}
	var hits []compose.Entry
	if in.Form {
		if in.Query == "" {
			return nil, grammarOut{}, fmt.Errorf("form=true requires a query")
		}
		hits = compose.LookupForm(in.Query)
		if in.Category != "" {
			hits = filterEntriesByCategory(hits, in.Category)
		}
	} else {
		hits = compose.Filter(in.Category, in.Query, in.Exact)
	}
	return nil, grammarOut{Entries: toGrammarEntries(hits)}, nil
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
// lexicon
// --------------------------------------------------------------------

type lexiconIn struct {
	Query   string   `json:"query,omitempty" jsonschema:"substring across surface clusters and meaning text"`
	Queries []string `json:"queries,omitempty" jsonschema:"batch: list of substrings; results deduplicated by cluster"`
	Kind    string   `json:"kind,omitempty" jsonschema:"root|affix|both (default both)"`
	Limit   int      `json:"limit,omitempty" jsonschema:"maximum hits per kind per query (default 20)"`
}

type rootHitOut struct {
	Cr           string   `json:"cr"`
	Score        int      `json:"score"`
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

type lexiconOut struct {
	Roots   []rootHitOut  `json:"roots,omitempty"`
	Affixes []affixHitOut `json:"affixes,omitempty"`
}

func (s *server) lexicon(_ context.Context, _ *mcp.CallToolRequest, in lexiconIn) (*mcp.CallToolResult, lexiconOut, error) {
	queries := in.Queries
	if q := strings.TrimSpace(in.Query); q != "" {
		queries = append(queries, q)
	}
	if len(queries) == 0 {
		return nil, lexiconOut{}, fmt.Errorf("query or queries is required")
	}
	limit := in.Limit
	if limit <= 0 {
		limit = 20
	}
	kind := in.Kind
	if kind == "" {
		kind = "both"
	}
	seenRoot := make(map[string]struct{})
	seenAffix := make(map[string]struct{})
	out := lexiconOut{}
	for _, q := range queries {
		if kind == "root" || kind == "both" {
			hits := compose.SearchRoots(q, s.lex.Roots)
			if len(hits) > limit {
				hits = hits[:limit]
			}
			for _, h := range hits {
				if _, seen := seenRoot[h.Cr]; seen {
					continue
				}
				seenRoot[h.Cr] = struct{}{}
				out.Roots = append(out.Roots, rootHitOut{
					Cr: h.Cr, Score: h.Score,
					Stem0: h.Entry.Stem0, Stem1: h.Entry.Stem1,
					Stem2: h.Entry.Stem2, Stem3: h.Entry.Stem3,
					Contential:   h.Entry.Contential,
					Constitutive: h.Entry.Constitutive,
					Objective:    h.Entry.Objective,
					Completive:   h.Entry.Completive,
					Dynamic:      h.Entry.Dynamic,
					Wikidata:     h.Entry.Wikidata,
				})
			}
		}
		if kind == "affix" || kind == "both" {
			hits := compose.SearchAffixes(q, s.lex.Affixes)
			if len(hits) > limit {
				hits = hits[:limit]
			}
			for _, a := range hits {
				if _, seen := seenAffix[a.Cs]; seen {
					continue
				}
				seenAffix[a.Cs] = struct{}{}
				out.Affixes = append(out.Affixes, toAffixHit(a))
			}
		}
	}
	return nil, out, nil
}

func toAffixHit(a lexicon.AffixEntry) affixHitOut {
	return affixHitOut{
		Cs: a.Cs, Abbrev: a.Abbrev, Description: a.Description,
		Type: a.Type, Degrees: a.Degrees,
	}
}

// --------------------------------------------------------------------
// validate
// --------------------------------------------------------------------

type validateIn struct {
	Text string `json:"text" jsonschema:"one or more space-separated Ithkuil words"`
}

type validateErrorOut struct {
	Word    string `json:"word"`
	Rule    string `json:"rule"`
	Cluster string `json:"cluster"`
	Reason  string `json:"reason"`
}

type validateOut struct {
	Valid  bool               `json:"valid"`
	Errors []validateErrorOut `json:"errors,omitempty"`
}

func (s *server) validate(_ context.Context, _ *mcp.CallToolRequest, in validateIn) (*mcp.CallToolResult, validateOut, error) {
	text := strings.TrimSpace(in.Text)
	if text == "" {
		return nil, validateOut{}, fmt.Errorf("text is required")
	}
	var allErrors []validateErrorOut
	for _, word := range strings.Fields(text) {
		res := validation.ValidateWord(word)
		for _, e := range res.Errors {
			allErrors = append(allErrors, validateErrorOut{
				Word: word, Rule: e.Rule, Cluster: e.Cluster, Reason: e.Reason,
			})
		}
	}
	return nil, validateOut{
		Valid:  len(allErrors) == 0,
		Errors: allErrors,
	}, nil
}
