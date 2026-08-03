package main

import (
	"context"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
)

// The tool handlers are the server. Everything above them is the SDK's
// JSON-RPC loop, which has its own tests upstream, so these call the
// handlers directly with the same arguments the transport would decode.
//
// Two servers, because half the behaviour worth checking is what a
// handler does without a data store: the command starts anyway on a
// machine with no store built, warns, and serves what it can.

func testServer(t *testing.T) *server {
	t.Helper()
	st, err := store.Open(store.DefaultPath())
	if err != nil {
		t.Skipf("no data store at %s; run tools/build_db.py", store.DefaultPath())
	}
	t.Cleanup(func() { st.Close() })
	lex, err := store.LoadLexicon(st)
	if err != nil {
		t.Fatalf("load lexicon: %v", err)
	}
	return &server{lex: lex, st: st, grammarDir: "../../../docs/reference"}
}

// emptyServer is what main builds when the store will not open: no
// store, and a lexicon with nothing in it.
func emptyServer() *server {
	return &server{lex: &lexicon.Lexicon{}, grammarDir: "../../../docs/reference"}
}

// ---- parse ----

func TestMCPParse_Formative(t *testing.T) {
	s := testServer(t)
	_, out, err := s.parse(context.Background(), nil, parseIn{Text: "malëuţřait"})
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(out.Words) != 1 {
		t.Fatalf("want 1 word, got %d", len(out.Words))
	}
	w := out.Words[0]
	if w.Type != "Form" {
		t.Errorf("type = %q, want Form", w.Type)
	}
	if !w.Valid {
		t.Errorf("the canonical test word is pronounceable; got violations %v", w.Violations)
	}
	if w.Gloss == "" || len(w.Segments) == 0 {
		t.Errorf("gloss %q, %d segments; want both", w.Gloss, len(w.Segments))
	}
	if w.Root == nil {
		t.Fatal("a formative has a root head")
	}
	// Without verbose the meanings stay out: they are the bulk of the
	// payload and a caller that only wants the slots pays for them.
	if w.Root.Meaning != "" {
		t.Errorf("root meaning %q leaked without verbose", w.Root.Meaning)
	}
	if len(w.Glossary) != 0 {
		t.Errorf("glossary present without verbose: %d rows", len(w.Glossary))
	}
}

func TestMCPParse_Verbose(t *testing.T) {
	s := testServer(t)
	_, out, err := s.parse(context.Background(), nil, parseIn{Text: "malëuţřait", Verbose: true})
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	w := out.Words[0]
	if w.Root == nil || w.Root.Meaning == "" {
		t.Error("verbose asks for the root definition")
	}
	if len(w.Glossary) == 0 {
		t.Error("verbose asks for the glossary")
	}
	for _, row := range w.Glossary {
		if row.Category == "" || row.Code == "" {
			t.Errorf("glossary row missing category or code: %+v", row)
		}
	}
}

// TestMCPParse_ASCII covers the digraph input path: a caller typing
// from a plain keyboard sends "malE'ut,r'ait" and must get the same
// reading as the Unicode form.
func TestMCPParse_ASCII(t *testing.T) {
	s := testServer(t)
	_, uni, err := s.parse(context.Background(), nil, parseIn{Text: "aţkuil"})
	if err != nil {
		t.Fatalf("parse unicode: %v", err)
	}
	_, ascii, err := s.parse(context.Background(), nil, parseIn{Text: "at,kuil"})
	if err != nil {
		t.Fatalf("parse ascii: %v", err)
	}
	if uni.Words[0].Gloss != ascii.Words[0].Gloss {
		t.Errorf("ascii and unicode disagree: %q vs %q",
			ascii.Words[0].Gloss, uni.Words[0].Gloss)
	}
}

// TestMCPParse_Unreadable checks the branch that exists so an
// unreadable word is not a blank: the type is "?", the reason says why,
// and the shape split survives even though the grammatical reading did
// not.
func TestMCPParse_Unreadable(t *testing.T) {
	s := testServer(t)
	_, out, err := s.parse(context.Background(), nil, parseIn{Text: "étkwö'e"})
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	w := out.Words[0]
	if w.Type != "?" {
		t.Fatalf("type = %q, want ?", w.Type)
	}
	if w.Reason == "" {
		t.Error("an unreadable word must say why")
	}
	if !strings.HasPrefix(w.Gloss, "?") {
		t.Errorf("gloss = %q, want a leading ?", w.Gloss)
	}
}

func TestMCPParse_Sentence(t *testing.T) {
	s := testServer(t)
	_, out, err := s.parse(context.Background(), nil, parseIn{Text: "hi malëuţřait"})
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(out.Words) != 2 {
		t.Fatalf("want 2 words, got %d", len(out.Words))
	}
	if out.Words[0].Type == out.Words[1].Type {
		t.Errorf("a register marker and a formative are different types; both %q",
			out.Words[0].Type)
	}
}

// TestMCPParse_Modular covers the second of the two word classes that
// get a slot breakdown. A modular adjunct's Slot 3 is Mood on a verbal
// formative and Case-Scope on a nominal one, a fact about the
// neighbours rather than the adjunct, so it is parsed inside a span
// where there is a neighbour to consult.
func TestMCPParse_Modular(t *testing.T) {
	s := testServer(t)
	_, out, err := s.parse(context.Background(), nil, parseIn{Text: "a malëuţřait", Verbose: true})
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(out.Words) != 2 {
		t.Fatalf("want 2 words, got %d", len(out.Words))
	}
	mod := out.Words[0]
	if mod.Type != "Mod" {
		t.Fatalf("type = %q, want Mod", mod.Type)
	}
	if len(mod.Segments) == 0 {
		t.Error("a modular adjunct reports its slots")
	}
	if len(mod.Glossary) == 0 {
		t.Error("verbose asks for the modular glossary too")
	}
}

// TestMCPParse_Violations covers the branch that reports what a word
// breaks. §4.5.4's carrier adjunct is the case: hňa is Quijada's own
// worked example and the word-initial cluster table rejects hň, which
// is filed as G45 in ISSUES.md. The word still classifies, so the
// caller gets a reading and the complaint together.
func TestMCPParse_Violations(t *testing.T) {
	s := testServer(t)
	_, out, err := s.parse(context.Background(), nil, parseIn{Text: "hňa"})
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	w := out.Words[0]
	if w.Type != "Carrier" {
		t.Fatalf("type = %q, want Carrier", w.Type)
	}
	if w.Valid {
		t.Fatal("hň is not licensed word-initially")
	}
	if len(w.Violations) == 0 {
		t.Fatal("an invalid word names the rules it breaks")
	}
	for _, v := range w.Violations {
		if v.Code == "" || v.Fix == "" {
			t.Errorf("violation missing the rule it breaks or what would fix it: %+v", v)
		}
	}
}

func TestMCPParse_EmptyText(t *testing.T) {
	s := emptyServer()
	if _, _, err := s.parse(context.Background(), nil, parseIn{Text: "  "}); err == nil {
		t.Error("empty text is an error")
	}
}

// ---- compare ----

func TestMCPCompare(t *testing.T) {
	s := testServer(t)
	_, out, err := s.compare(context.Background(), nil, compareIn{A: "malëuţřait", B: "maţřëullait"})
	if err != nil {
		t.Fatalf("compare: %v", err)
	}
	if out.A == "" || out.B == "" {
		t.Errorf("both words should be echoed; got %q and %q", out.A, out.B)
	}
	if len(out.Pairs) == 0 {
		t.Fatal("two formatives pair with each other")
	}
	// The two spellings of the v4 nickname are the same morphemes with
	// the SYS affix in a different slot, so they pair and differ.
	pair := out.Pairs[0]
	if len(pair.Slots) == 0 {
		t.Error("a pair reports its slots")
	}
	if pair.Identical {
		t.Error("these two differ in which slot carries SYS")
	}
}

func TestMCPCompare_Identical(t *testing.T) {
	s := testServer(t)
	_, out, err := s.compare(context.Background(), nil, compareIn{A: "mala", B: "mala"})
	if err != nil {
		t.Fatalf("compare: %v", err)
	}
	if len(out.Pairs) == 0 || !out.Pairs[0].Identical {
		t.Errorf("a word compared with itself is identical; got %+v", out.Pairs)
	}
}

func TestMCPCompare_Errors(t *testing.T) {
	s := testServer(t)
	cases := []struct {
		name string
		in   compareIn
		want string
	}{
		{"empty a", compareIn{A: "", B: "mala"}, "required"},
		{"empty b", compareIn{A: "mala", B: ""}, "required"},
		{"unpronounceable", compareIn{A: "akxq", B: "mala"}, "not pronounceable"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			_, _, err := s.compare(context.Background(), nil, c.in)
			if err == nil {
				t.Fatalf("want an error containing %q", c.want)
			}
			if !strings.Contains(err.Error(), c.want) {
				t.Errorf("error %q does not mention %q", err, c.want)
			}
		})
	}
}

// ---- compose ----

func TestMCPCompose_Formative(t *testing.T) {
	s := testServer(t)
	_, out, err := s.compose(context.Background(), nil, composeIn{Expression: "S2.CPT-ml-ERG"})
	if err != nil {
		t.Fatalf("compose: %v", err)
	}
	if out.Romanization != "wimlo" {
		t.Errorf("romanization = %q, want wimlo", out.Romanization)
	}
	if len(out.Segments) == 0 {
		t.Error("a composed formative reports its slots")
	}
	if out.Root == nil {
		t.Fatal("a composed formative has a root head")
	}
	if out.Root.Meaning != "" || len(out.Glossary) != 0 {
		t.Error("meanings and glossary belong to verbose")
	}
}

// TestMCPCompose_RoundTrip is the property the tool exists for: what it
// writes must gloss back to what was asked for.
func TestMCPCompose_RoundTrip(t *testing.T) {
	s := testServer(t)
	for _, expr := range []string{"ml", "S2.CPT-ml-ERG", "DYN-ml-THM", "[1m]-THM"} {
		_, out, err := s.compose(context.Background(), nil, composeIn{Expression: expr})
		if err != nil {
			t.Errorf("compose(%q): %v", expr, err)
			continue
		}
		_, back, err := s.parse(context.Background(), nil, parseIn{Text: out.Romanization})
		if err != nil {
			t.Errorf("parse(%q): %v", out.Romanization, err)
			continue
		}
		if got := back.Words[0].Gloss; got != out.Gloss {
			t.Errorf("compose(%q) -> %q glosses to %q, not the %q it was composed as",
				expr, out.Romanization, got, out.Gloss)
		}
	}
}

// TestMCPCompose_NonFormative covers the early return: the other word
// classes have their own shapes and no slot breakdown to show.
func TestMCPCompose_NonFormative(t *testing.T) {
	s := testServer(t)
	_, out, err := s.compose(context.Background(), nil, composeIn{Expression: "DPB"})
	if err != nil {
		t.Fatalf("compose: %v", err)
	}
	if out.Romanization == "" {
		t.Error("a bias adjunct is a word")
	}
	if out.Segments != nil || out.Root != nil {
		t.Errorf("a bias has no slots or root; got %+v", out)
	}
}

func TestMCPCompose_Verbose(t *testing.T) {
	s := testServer(t)
	_, out, err := s.compose(context.Background(), nil, composeIn{Expression: "S2.CPT-ml-ERG", Verbose: true})
	if err != nil {
		t.Fatalf("compose: %v", err)
	}
	if out.Root == nil || out.Root.Meaning == "" || len(out.Glossary) == 0 {
		t.Error("verbose asks for the root definition and the glossary")
	}
}

func TestMCPCompose_Errors(t *testing.T) {
	s := testServer(t)
	for _, c := range []struct{ name, expr string }{
		{"empty", "   "},
		{"nonsense", "not-a-gloss-at-all"},
	} {
		t.Run(c.name, func(t *testing.T) {
			if _, _, err := s.compose(context.Background(), nil, composeIn{Expression: c.expr}); err == nil {
				t.Error("want an error")
			}
		})
	}
}

// ---- search ----

func TestMCPSearch_NoQueryListsCategories(t *testing.T) {
	s := emptyServer()
	_, out, err := s.search(context.Background(), nil, searchIn{})
	if err != nil {
		t.Fatalf("search: %v", err)
	}
	if len(out.Categories) == 0 {
		t.Fatal("a bare search lists the categories")
	}
	if len(out.Entries) != 0 || len(out.Roots) != 0 {
		t.Error("a bare search returns categories and nothing else")
	}
}

func TestMCPSearch_Abbrev(t *testing.T) {
	s := testServer(t)
	_, out, err := s.search(context.Background(), nil, searchIn{Query: "CAR", Exact: true})
	if err != nil {
		t.Fatalf("search: %v", err)
	}
	var found bool
	for _, e := range out.Entries {
		if e.Category == "CarrierType" && e.Abbrev == "CAR" {
			found = true
			if e.Name == "" || e.Description == "" {
				t.Errorf("CAR has a name and a description; got %+v", e)
			}
		}
	}
	if !found {
		t.Errorf("CAR not among %d entries", len(out.Entries))
	}
}

// TestMCPSearch_Form asks what a written form encodes, which is a
// grammar question: the lexicon has no answer to what a vowel means, so
// the handler returns early without touching the store.
func TestMCPSearch_Form(t *testing.T) {
	s := testServer(t)
	_, out, err := s.search(context.Background(), nil, searchIn{Query: "hl", Form: true})
	if err != nil {
		t.Fatalf("search: %v", err)
	}
	if len(out.Entries) == 0 {
		t.Fatal("hl is the CAR carrier form")
	}
	if len(out.Roots) != 0 || len(out.Affixes) != 0 {
		t.Error("a form search does not reach the lexicon")
	}
}

// TestMCPSearch_FormWithCategory covers both arms of
// filterEntriesByCategory, which no written form exercises on its own:
// every form in the table belongs to exactly one category, so a hit is
// either kept by a matching filter or dropped by a mismatched one.
func TestMCPSearch_FormWithCategory(t *testing.T) {
	s := testServer(t)
	_, kept, err := s.search(context.Background(), nil, searchIn{Query: "a", Form: true, Category: "Case"})
	if err != nil {
		t.Fatalf("search: %v", err)
	}
	if len(kept.Entries) == 0 {
		t.Fatal("a is the THM case vowel and Case is its category")
	}
	for _, e := range kept.Entries {
		if !strings.HasPrefix(e.Category, "Case") {
			t.Errorf("%s/%s survived a Case filter", e.Category, e.Abbrev)
		}
	}
	_, dropped, err := s.search(context.Background(), nil, searchIn{Query: "a", Form: true, Category: "Bias"})
	if err != nil {
		t.Fatalf("search: %v", err)
	}
	if len(dropped.Entries) != 0 {
		t.Errorf("a is not a bias form; got %+v", dropped.Entries)
	}
}

func TestMCPSearch_FormNeedsQuery(t *testing.T) {
	s := emptyServer()
	_, _, err := s.search(context.Background(), nil, searchIn{Form: true, Category: "Case"})
	if err == nil {
		t.Fatal("form=true without a query is an error")
	}
}

func TestMCPSearch_Lexicon(t *testing.T) {
	s := testServer(t)
	_, out, err := s.search(context.Background(), nil, searchIn{Query: "water", Limit: 5})
	if err != nil {
		t.Fatalf("search: %v", err)
	}
	if len(out.Roots) == 0 {
		t.Fatal("water is in the lexicon")
	}
	if len(out.Roots) > 5 {
		t.Errorf("limit 5 returned %d roots", len(out.Roots))
	}
	for _, r := range out.Roots {
		if r.Cr == "" {
			t.Errorf("root hit with no cluster: %+v", r)
		}
	}
}

// TestMCPSearch_NoStore covers the path main takes when the store will
// not open: grammar lookup still works, since that table is in the
// binary, and the lexicon half says so rather than returning empty.
func TestMCPSearch_NoStore(t *testing.T) {
	s := emptyServer()
	_, out, err := s.search(context.Background(), nil, searchIn{Query: "ERG"})
	if err == nil {
		t.Fatal("want an error naming the missing store")
	}
	if !strings.Contains(err.Error(), "data store") {
		t.Errorf("error %q does not mention the store", err)
	}
	if len(out.Entries) == 0 {
		t.Error("the grammar table is in the binary and should still answer")
	}
}

// TestMCPSearch_CategoryOnly covers the branch where a category is given
// with no query: grammar entries only, and the store is never asked.
func TestMCPSearch_CategoryOnly(t *testing.T) {
	s := emptyServer()
	_, out, err := s.search(context.Background(), nil, searchIn{Category: "Bias"})
	if err != nil {
		t.Fatalf("search: %v", err)
	}
	if len(out.Entries) == 0 {
		t.Fatal("Bias has 61 entries")
	}
	for _, e := range out.Entries {
		if e.Category != "Bias" {
			t.Errorf("%s survived a Bias filter", e.Category)
		}
	}
}

// ---- define ----

func TestMCPDefine(t *testing.T) {
	s := testServer(t)
	_, out, err := s.define(context.Background(), nil, defineIn{Word: "water"})
	if err != nil {
		t.Fatalf("define: %v", err)
	}
	if out.Word != "water" {
		t.Errorf("word = %q", out.Word)
	}
	if len(out.Senses) == 0 {
		t.Fatal("water has at least one sense")
	}
	for _, sense := range out.Senses {
		if sense.Romanization == "" || sense.Gloss == "" || sense.Meaning == "" {
			t.Errorf("incomplete sense: %+v", sense)
		}
	}
}

// TestMCPDefine_Limit covers the truncation branch and the More count
// that tells the caller how much was left behind.
func TestMCPDefine_Limit(t *testing.T) {
	s := testServer(t)
	_, all, err := s.define(context.Background(), nil, defineIn{Word: "go", Limit: 100})
	if err != nil {
		t.Fatalf("define: %v", err)
	}
	if len(all.Senses) < 2 {
		t.Skip("need a word with several senses to test truncation")
	}
	_, one, err := s.define(context.Background(), nil, defineIn{Word: "go", Limit: 1})
	if err != nil {
		t.Fatalf("define: %v", err)
	}
	if len(one.Senses) != 1 {
		t.Fatalf("limit 1 returned %d senses", len(one.Senses))
	}
	if want := len(all.Senses) - 1; one.More != want {
		t.Errorf("more = %d, want %d", one.More, want)
	}
}

func TestMCPDefine_Unknown(t *testing.T) {
	s := testServer(t)
	_, out, err := s.define(context.Background(), nil, defineIn{Word: "qwertyuiop"})
	if err != nil {
		t.Fatalf("an unknown word is not an error: %v", err)
	}
	if len(out.Senses) != 0 {
		t.Errorf("want no senses, got %d", len(out.Senses))
	}
}

func TestMCPDefine_Errors(t *testing.T) {
	if _, _, err := emptyServer().define(context.Background(), nil, defineIn{Word: " "}); err == nil {
		t.Error("empty word is an error")
	}
	s := &server{}
	if _, _, err := s.define(context.Background(), nil, defineIn{Word: "water"}); err == nil {
		t.Error("no lexicon is an error")
	}
}
