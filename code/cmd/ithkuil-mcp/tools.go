package main

import (
	"context"
	"fmt"
	"strings"

	"github.com/modelcontextprotocol/go-sdk/mcp"

	"github.com/christian-oudard/ithkuil/api"
	"github.com/christian-oudard/ithkuil/phonology"
)

// The tools answer in the api package's types rather than a set of
// their own. They used to declare seventeen: segmentOut, slotRowOut,
// rootHitOut, senseOut and the rest, each a near-copy of a type in view
// or lexicon, each mapped across by hand. The browser module then grew
// a second set of copies, and the two disagreed on names that should
// have been the same word, `default` against `defaults`, snake_case
// against camelCase. One shape for one thing, and the drift guard in
// api keeps the TypeScript declaration honest about all of it.
//
// What stays here is what is genuinely about serving a model rather
// than a program: the verbose switch, which strips the meanings and the
// glossary from a reply because they are most of its tokens and a
// caller that wants them can ask the search tool instead.

// registerTools wires every tool to its handler. The set mirrors the
// ithkuil CLI subcommands one-for-one: parse, compare, compose, search,
// define.
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
			"Example: a=\"marçat\", b=\"marcat\".",
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
			"canonical. Set stressless=true to write stress as a §4.8 parsing adjunct " +
			"instead of a diacritic. By default (verbose=false) descriptions are omitted. Set " +
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

type parseOut struct {
	Words []api.Word `json:"words"`
}

func (s *server) parse(_ context.Context, _ *mcp.CallToolRequest, in parseIn) (*mcp.CallToolResult, parseOut, error) {
	text := strings.TrimSpace(in.Text)
	if text == "" {
		return nil, parseOut{}, fmt.Errorf("text is required")
	}
	words := s.api.Parse(text)
	for i := range words {
		trim(&words[i], in.Verbose)
	}
	return nil, parseOut{Words: words}, nil
}

// trim drops what a model does not need to be told twice. The glossary
// and the meanings are most of a reply's tokens, and a caller that
// wants them can ask the search tool for the one code it cares about.
// The gloss tokens go always: they are the same line the gloss already
// carries, split for a page to make clickable, which is no use here.
func trim(w *api.Word, verbose bool) {
	w.GlossTokens = nil
	for i := range w.Members {
		w.Members[i].Glossary = trimGlossary(w.Members[i].Glossary, verbose)
		w.Members[i].Headword = trimHead(w.Members[i].Headword, verbose)
	}
	w.Glossary = trimGlossary(w.Glossary, verbose)
	w.Headword = trimHead(w.Headword, verbose)
}

func trimGlossary(rows []api.GlossaryEntry, verbose bool) []api.GlossaryEntry {
	if verbose {
		return rows
	}
	return nil
}

func trimHead(h *api.Headword, verbose bool) *api.Headword {
	if h == nil || verbose {
		return h
	}
	return &api.Headword{Code: h.Code}
}

// --------------------------------------------------------------------
// compare
// --------------------------------------------------------------------

type compareIn struct {
	A string `json:"a" jsonschema:"the first word"`
	B string `json:"b" jsonschema:"the second word"`
}

func (s *server) compare(_ context.Context, _ *mcp.CallToolRequest, in compareIn) (*mcp.CallToolResult, api.Comparison, error) {
	a, err := pronounceable(in.A)
	if err != nil {
		return nil, api.Comparison{}, err
	}
	b, err := pronounceable(in.B)
	if err != nil {
		return nil, api.Comparison{}, err
	}
	out, err := s.api.Compare(a, b)
	if err != nil {
		return nil, api.Comparison{}, err
	}
	return nil, out, nil
}

// pronounceable refuses a word that breaks §2 before anything compares
// it, naming the rule, which is the same refusal the CLI makes. A
// comparison of two words that cannot be said is not a useful answer.
func pronounceable(word string) (string, error) {
	word = phonology.FromASCII(strings.TrimSpace(word))
	if word == "" {
		return "", fmt.Errorf("both a and b are required")
	}
	if err := phonology.CheckText(word); err != nil {
		return "", fmt.Errorf("%s is not pronounceable Ithkuil: %w", word, err)
	}
	return word, nil
}

// --------------------------------------------------------------------
// compose
// --------------------------------------------------------------------

type composeIn struct {
	Expression string `json:"expression" jsonschema:"gloss-style compose expression; '-' separates slots, '.' joins category values in a slot, '/' binds a degree or case to a head; affixes before the Ca land in Slot V, with '{Ca}' marking an all-default Ca; bare cluster like 'ml' or full 'S2.CPT-ml-DYN.OBJ-MSS.G-DEV/3-ERG'"`
	Stressless bool   `json:"stressless,omitempty" jsonschema:"write stress as a §4.8 parsing adjunct instead of a diacritic (default false)"`
	Verbose    bool   `json:"verbose,omitempty" jsonschema:"include category names, meanings, and root definition (default false)"`
}

// compose answers with the word it built, read back. The reply is an
// api.Word because that is already what one is: the old composeOut had
// romanization, gloss, root, segments and glossary, which is the same
// five fields under different names.
//
// Reading it back rather than reporting what the builder held is
// deliberate. It is the round trip the tool exists to guarantee, so the
// breakdown shown is evidence about the word that was written and not
// about the intent behind it.
func (s *server) compose(_ context.Context, _ *mcp.CallToolRequest, in composeIn) (*mcp.CallToolResult, api.Word, error) {
	expr := strings.TrimSpace(in.Expression)
	if expr == "" {
		return nil, api.Word{}, fmt.Errorf("expression is required")
	}
	built, err := s.api.Compose(expr, in.Stressless)
	if err != nil {
		return nil, api.Word{}, err
	}
	words := s.api.Parse(built.Word)
	if len(words) != 1 {
		// A word that will not read back is a defect in this code, not
		// a bad request, and saying only "romanization: x" would hide
		// it. Report what was built and let the caller see the gap.
		return nil, api.Word{Romanization: built.Word, Gloss: built.Gloss}, nil
	}
	trim(&words[0], in.Verbose)
	return nil, words[0], nil
}

// --------------------------------------------------------------------
// search
// --------------------------------------------------------------------

type searchIn struct {
	Query    string `json:"query,omitempty" jsonschema:"term to look up: an abbreviation, a name, or an English keyword"`
	Category string `json:"category,omitempty" jsonschema:"list only this grammar category (Case, Aspect, Bias, Mood, ...)"`
	Exact    bool   `json:"exact,omitempty" jsonschema:"the query must equal an abbreviation"`
	Form     bool   `json:"form,omitempty" jsonschema:"treat the query as a written form (vowel or consonant)"`
	Limit    int    `json:"limit,omitempty" jsonschema:"maximum lexicon hits per kind (default 20)"`
}

type searchOut struct {
	api.SearchResult
	// Categories answers a call with nothing to search for, so a caller
	// can discover what --category takes.
	Categories []string `json:"categories,omitempty"`
}

func (s *server) search(_ context.Context, _ *mcp.CallToolRequest, in searchIn) (*mcp.CallToolResult, searchOut, error) {
	query := strings.TrimSpace(in.Query)
	if query == "" && in.Category == "" {
		return nil, searchOut{Categories: s.api.Categories()}, nil
	}
	if in.Form && query == "" {
		return nil, searchOut{}, fmt.Errorf("form=true requires a query")
	}
	got := s.api.Search(query, api.SearchOptions{
		Category: in.Category,
		Exact:    in.Exact,
		Form:     in.Form,
		Limit:    in.Limit,
	})
	out := searchOut{SearchResult: got}
	// An empty lexicon half means one of two things and a caller cannot
	// tell them apart from the reply: no root matched, or there is no
	// store to match against. Say which.
	if s.st == nil && query != "" && in.Category == "" && !in.Form {
		return nil, out, fmt.Errorf("data store not available: grammar hits only")
	}
	return nil, out, nil
}

// --------------------------------------------------------------------
// define
// --------------------------------------------------------------------

type defineIn struct {
	Word  string `json:"word" jsonschema:"an English word to look up"`
	Limit int    `json:"limit,omitempty" jsonschema:"maximum senses returned (default 20)"`
}

func (s *server) define(_ context.Context, _ *mcp.CallToolRequest, in defineIn) (*mcp.CallToolResult, api.Definition, error) {
	word := strings.TrimSpace(in.Word)
	if word == "" {
		return nil, api.Definition{}, fmt.Errorf("word is required")
	}
	out, err := s.api.Define(word, in.Limit)
	if err != nil {
		return nil, api.Definition{}, err
	}
	return nil, out, nil
}
