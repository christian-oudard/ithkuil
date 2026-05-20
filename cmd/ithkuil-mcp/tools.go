package main

import (
	"context"
	"fmt"
	"strings"

	"github.com/modelcontextprotocol/go-sdk/mcp"

	"github.com/christian-oudard/ithkuil/compose"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/inspect"
	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/tokenize"
	"github.com/christian-oudard/ithkuil/validation"
)

// registerTools wires every tool to its handler.
func (s *server) registerTools(srv *mcp.Server) {
	mcp.AddTool(srv, &mcp.Tool{
		Name: "analyze",
		Description: "Tokenize, parse, and gloss every word in the given Ithkuil text. " +
			"For each word returns surface, type (Form/Ref/Bias/...), one-line gloss, " +
			"a slot map (only non-default slots), and per-word phonotactic validation. " +
			"The canonical \"what does this mean\" call. Example: text=\"Malëuţřait\".",
	}, s.analyze)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "compose",
		Description: "Build a surface Ithkuil formative from a root and structured grammar " +
			"choices. Every field except root is optional; omitted slots take grammatical " +
			"defaults (S1 stem, PRC version, STA function, BSC spec, EXS context, THM case, " +
			"PEN stress). Use any of the standard 3-letter abbreviations for values. " +
			"Example: root=\"ml\", stem=\"S2\", version=\"CPT\", case=\"ERG\" → \"imlalo\".",
	}, s.compose)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "diff",
		Description: "Slot-by-slot diff between two formatives or two aligned sentences. " +
			"Returns one row per slot (label, A value, B value, changed flag) for each " +
			"paired word, plus a formatted text view. Words present only on one side are " +
			"listed as a_only/b_only. Example: a=\"arralo\", b=\"erralo\" → Slot II changed.",
	}, s.diff)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "grammar",
		Description: "Look up the grammar inventory. With no args, lists every category " +
			"name. With category, lists all entries in that category (Case, Aspect, Bias, " +
			"Mood, ...). With query, substring matches against abbreviation / category / " +
			"surface form / description. With exact=true, exact abbrev match. With " +
			"form=true, treats query as a surface form (vowel or consonant). Both " +
			"category and query may be combined. Example: category=\"Bias\", query=\"please\".",
	}, s.grammar)

	mcp.AddTool(srv, &mcp.Tool{
		Name: "lexicon",
		Description: "Substring search the root and/or affix lexicons. Pass kind=\"root\", " +
			"\"affix\", or \"both\" (default both). Returns ranked hits with surface " +
			"clusters and meaning text. Example: query=\"speak\", kind=\"root\".",
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
	Text string `json:"text" jsonschema:"one or more Ithkuil words"`
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
			Type:    inspect.Type(t),
			Gloss:   glosser.Token(t),
		}
		switch tt := t.(type) {
		case tokenize.FormativeWord:
			head := inspect.Headword(tt.Formative, s.lex)
			if head.Code != "" {
				w.Root = &rootHead{Code: head.Code, Meaning: head.Meaning}
			}
			segs := inspect.Segments(tt.Text, tt.Formative, s.lex)
			for _, sg := range segs {
				w.Segments = append(w.Segments, segmentOut{
					Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
					Encodes: sg.Encodes, Default: sg.Defaults, Elided: sg.Elided,
				})
			}
			for _, ge := range inspect.Glossary(tt.Text, tt.Formative, segs, s.lex) {
				w.Glossary = append(w.Glossary, glossaryRow{
					Category: ge.Category, Code: ge.Code,
					Name: ge.Name, Meaning: ge.Meaning,
				})
			}
		case tokenize.ModularWord:
			segs := inspect.SegmentsModular(tt.Text, tt.Modular)
			for _, sg := range segs {
				w.Segments = append(w.Segments, segmentOut{
					Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
					Encodes: sg.Encodes, Default: sg.Defaults, Elided: sg.Elided,
				})
			}
			for _, ge := range inspect.GlossaryModular(segs) {
				w.Glossary = append(w.Glossary, glossaryRow{
					Category: ge.Category, Code: ge.Code,
					Name: ge.Name, Meaning: ge.Meaning,
				})
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
	Root          string `json:"root" jsonschema:"the root consonant cluster (Cr)"`
	Stem          string `json:"stem,omitempty" jsonschema:"S0|S1|S2|S3 (default S1)"`
	Version       string `json:"version,omitempty" jsonschema:"PRC|CPT (default PRC)"`
	Function      string `json:"function,omitempty" jsonschema:"STA|DYN (default STA)"`
	Specification string `json:"specification,omitempty" jsonschema:"BSC|CTE|CSV|OBJ (default BSC)"`
	Context       string `json:"context,omitempty" jsonschema:"EXS|FNC|RPS|AMG (default EXS)"`
	Case          string `json:"case,omitempty" jsonschema:"any of the 68 cases (default THM)"`
	Aspect        string `json:"aspect,omitempty" jsonschema:"Slot VIII aspect"`
	Valence       string `json:"valence,omitempty" jsonschema:"Slot VIII valence"`
	Mood          string `json:"mood,omitempty" jsonschema:"Slot VIII mood"`
	Illocution    string `json:"illocution,omitempty" jsonschema:"Slot IX illocution (forces ULT stress)"`
	Stress        string `json:"stress,omitempty" jsonschema:"MON|PEN|ULT|ANT (default PEN)"`
}

type composeOut struct {
	Surface  string        `json:"surface"`
	Gloss    string        `json:"gloss"`
	Root     *rootHead     `json:"root,omitempty"`
	Segments []segmentOut  `json:"segments,omitempty"`
	Glossary []glossaryRow `json:"glossary,omitempty"`
}

func (s *server) compose(_ context.Context, _ *mcp.CallToolRequest, in composeIn) (*mcp.CallToolResult, composeOut, error) {
	root := strings.TrimSpace(in.Root)
	if root == "" {
		return nil, composeOut{}, fmt.Errorf("root is required")
	}
	f := g.MinimalFormative(root)
	for _, v := range []string{
		in.Stem, in.Version, in.Function, in.Specification, in.Context,
		in.Case, in.Aspect, in.Valence, in.Mood, in.Illocution, in.Stress,
	} {
		if v == "" {
			continue
		}
		if err := compose.ApplyFlag(&f, v); err != nil {
			return nil, composeOut{}, err
		}
	}
	surface := render.Formative(f)
	glosser := gloss.Glosser{Lex: s.lex}
	segs := inspect.Segments(surface, f, s.lex)
	head := inspect.Headword(f, s.lex)
	out := composeOut{
		Surface: surface,
		Gloss:   glosser.Formative(f),
	}
	if head.Code != "" {
		out.Root = &rootHead{Code: head.Code, Meaning: head.Meaning}
	}
	for _, sg := range segs {
		out.Segments = append(out.Segments, segmentOut{
			Chunk: sg.Chunk, Raw: sg.Raw, Slot: sg.Slot,
			Encodes: sg.Encodes, Default: sg.Defaults, Elided: sg.Elided,
		})
	}
	for _, ge := range inspect.Glossary(surface, f, segs, s.lex) {
		out.Glossary = append(out.Glossary, glossaryRow{
			Category: ge.Category, Code: ge.Code,
			Name: ge.Name, Meaning: ge.Meaning,
		})
	}
	return nil, out, nil
}

// --------------------------------------------------------------------
// diff
// --------------------------------------------------------------------

type diffIn struct {
	A string `json:"a" jsonschema:"first formative or sentence"`
	B string `json:"b" jsonschema:"second formative or sentence"`
}

type diffRowOut struct {
	Label   string `json:"label"`
	A       string `json:"a"`
	B       string `json:"b"`
	Changed bool   `json:"changed"`
}

type diffWordOut struct {
	A    string       `json:"a"`
	B    string       `json:"b"`
	Rows []diffRowOut `json:"rows"`
}

type diffOut struct {
	Words     []diffWordOut `json:"words"`
	AOnly     []string      `json:"a_only,omitempty"`
	BOnly     []string      `json:"b_only,omitempty"`
	Formatted string        `json:"formatted"`
}

func (s *server) diff(_ context.Context, _ *mcp.CallToolRequest, in diffIn) (*mcp.CallToolResult, diffOut, error) {
	if strings.TrimSpace(in.A) == "" || strings.TrimSpace(in.B) == "" {
		return nil, diffOut{}, fmt.Errorf("both a and b are required")
	}
	lhs := tokenize.Tokenize(in.A)
	rhs := tokenize.Tokenize(in.B)

	pairs := len(lhs)
	if len(rhs) < pairs {
		pairs = len(rhs)
	}
	words := make([]diffWordOut, pairs)
	for i := 0; i < pairs; i++ {
		rows := inspect.DiffRows(lhs[i], rhs[i])
		out := make([]diffRowOut, len(rows))
		for j, r := range rows {
			out[j] = diffRowOut{Label: r.Label, A: r.A, B: r.B, Changed: r.A != r.B}
		}
		words[i] = diffWordOut{A: lhs[i].Surface(), B: rhs[i].Surface(), Rows: out}
	}
	var aOnly, bOnly []string
	for i := pairs; i < len(lhs); i++ {
		aOnly = append(aOnly, lhs[i].Surface())
	}
	for i := pairs; i < len(rhs); i++ {
		bOnly = append(bOnly, rhs[i].Surface())
	}
	var buf strings.Builder
	inspect.Diff(&buf, lhs, rhs)
	return nil, diffOut{
		Words:     words,
		AOnly:     aOnly,
		BOnly:     bOnly,
		Formatted: buf.String(),
	}, nil
}

// --------------------------------------------------------------------
// grammar
// --------------------------------------------------------------------

type grammarIn struct {
	Query    string `json:"query,omitempty" jsonschema:"abbreviation, category, form, or description substring"`
	Category string `json:"category,omitempty" jsonschema:"restrict to one category (Case, Aspect, Bias, ...)"`
	Exact    bool   `json:"exact,omitempty" jsonschema:"if true, query must equal Abbrev exactly"`
	Form     bool   `json:"form,omitempty" jsonschema:"if true, treat query as a surface form (vowel or consonant)"`
}

type grammarEntryOut struct {
	Category    string `json:"category"`
	Abbrev      string `json:"abbrev"`
	Form        string `json:"form,omitempty"`
	Description string `json:"description,omitempty"`
}

type grammarOut struct {
	Categories []string          `json:"categories,omitempty"`
	Entries    []grammarEntryOut `json:"entries,omitempty"`
}

func (s *server) grammar(_ context.Context, _ *mcp.CallToolRequest, in grammarIn) (*mcp.CallToolResult, grammarOut, error) {
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
	out := make([]grammarEntryOut, len(hits))
	for i, e := range hits {
		out[i] = grammarEntryOut{
			Category:    e.Category,
			Abbrev:      e.Abbrev,
			Form:        e.Form,
			Description: e.Description,
		}
	}
	return nil, grammarOut{Entries: out}, nil
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
	Query string `json:"query" jsonschema:"substring across surface clusters and meaning text"`
	Kind  string `json:"kind,omitempty" jsonschema:"root|affix|both (default both)"`
	Limit int    `json:"limit,omitempty" jsonschema:"maximum hits per kind (default 20)"`
}

type rootHitOut struct {
	Cr    string `json:"cr"`
	Score int    `json:"score"`
	Stem0 string `json:"stem0,omitempty"`
	Stem1 string `json:"stem1,omitempty"`
	Stem2 string `json:"stem2,omitempty"`
	Stem3 string `json:"stem3,omitempty"`
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
	q := strings.TrimSpace(in.Query)
	if q == "" {
		return nil, lexiconOut{}, fmt.Errorf("query is required")
	}
	limit := in.Limit
	if limit <= 0 {
		limit = 20
	}
	kind := in.Kind
	if kind == "" {
		kind = "both"
	}
	out := lexiconOut{}
	if kind == "root" || kind == "both" {
		hits := compose.SearchRoots(q, s.lex.Roots)
		if len(hits) > limit {
			hits = hits[:limit]
		}
		for _, h := range hits {
			out.Roots = append(out.Roots, rootHitOut{
				Cr: h.Cr, Score: h.Score,
				Stem0: h.Entry.Stem0, Stem1: h.Entry.Stem1,
				Stem2: h.Entry.Stem2, Stem3: h.Entry.Stem3,
			})
		}
	}
	if kind == "affix" || kind == "both" {
		hits := compose.SearchAffixes(q, s.lex.Affixes)
		if len(hits) > limit {
			hits = hits[:limit]
		}
		for _, a := range hits {
			out.Affixes = append(out.Affixes, toAffixHit(a))
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
