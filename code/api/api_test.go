package api

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"

	"github.com/christian-oudard/ithkuil/corpus"
	"testing"
)

func dataJSON(t *testing.T) []byte {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("..", "..", "data", "data.json"))
	if err != nil {
		t.Skipf("data.json not readable: %v", err)
	}
	return b
}

// loaded returns an API with the whole lexicon in it.
func loaded(t *testing.T) *API {
	t.Helper()
	a := New()
	if _, err := a.Load(dataJSON(t)); err != nil {
		t.Fatalf("Load: %v", err)
	}
	return a
}

// TestParse_NoLexicon pins the reason Load is a separate call. A page
// can show a word's slots while 1.8 MB of meanings is still in flight,
// so parsing must not need the lexicon at all.
func TestParse_NoLexicon(t *testing.T) {
	words := New().Parse("Maţřëullait")
	if len(words) != 1 {
		t.Fatalf("got %d words, want 1", len(words))
	}
	w := words[0]
	if w.Error != "" {
		t.Fatalf("parse failed without a lexicon: %s", w.Error)
	}
	if w.Gloss == "" || len(w.Segments) == 0 {
		t.Errorf("no gloss or segments without a lexicon: %+v", w)
	}
	// Meanings are the part that needs one.
	for _, e := range w.Glossary {
		if e.Code == "" {
			t.Errorf("glossary row with no code: %+v", e)
		}
	}
}

func TestParse_WithLexicon(t *testing.T) {
	w := loaded(t).Parse("Maţřëullait")[0]
	if w.Type != "Form" {
		t.Errorf("Type = %q, want Form", w.Type)
	}
	if w.Headword == nil || !strings.Contains(w.Headword.Meaning, "utterance") {
		t.Errorf("Headword = %+v, want the lexical meaning of m", w.Headword)
	}
	if len(w.Glossary) == 0 {
		t.Fatal("no glossary")
	}
	if w.Glossary[0].Meaning == "" {
		t.Errorf("glossary row has no meaning with a lexicon loaded: %+v", w.Glossary[0])
	}
}

// TestParse_FailureInPlace pins that a word that will not read stays in
// the answer carrying its reason, rather than failing the whole span.
// The page marks it and keeps the rest.
func TestParse_FailureInPlace(t *testing.T) {
	words := New().Parse("Maţřëullait xxxx Maţřëullait")
	if len(words) != 3 {
		t.Fatalf("got %d words, want 3", len(words))
	}
	if words[1].Error == "" {
		t.Error("the unreadable word reports no error")
	}
	if words[0].Error != "" || words[2].Error != "" {
		t.Error("one unreadable word cost its neighbours")
	}
}

// TestParse_ASCIIDigraphs pins that the input method is on by default.
// Nobody has these characters on a keyboard.
func TestParse_ASCIIDigraphs(t *testing.T) {
	got := New().Parse("mat,rqeullait")[0].Romanization
	if !strings.ContainsAny(got, "ţř") {
		t.Errorf("Romanization = %q, want the digraphs folded to ţ and ř", got)
	}
}

// TestLoad_Merges is the whole reason Load merges rather than replaces:
// a page fetches the 54 KB of affixes and the 260 KB of roots
// separately, and the second call must not drop what the first brought.
func TestLoad_Merges(t *testing.T) {
	var doc struct {
		Version uint16            `json:"version"`
		Roots   []json.RawMessage `json:"roots"`
		Affixes []json.RawMessage `json:"affixes"`
	}
	if err := json.Unmarshal(dataJSON(t), &doc); err != nil {
		t.Fatalf("splitting data.json: %v", err)
	}
	affixesOnly, err := json.Marshal(map[string]any{"affixes": doc.Affixes})
	if err != nil {
		t.Fatal(err)
	}
	rootsOnly, err := json.Marshal(map[string]any{"version": doc.Version, "roots": doc.Roots})
	if err != nil {
		t.Fatal(err)
	}

	a := New()
	info, err := a.Load(affixesOnly)
	if err != nil {
		t.Fatalf("Load(affixes): %v", err)
	}
	if info.Lexicon.Affixes == 0 || info.Lexicon.Roots != 0 {
		t.Fatalf("after affixes: %+v, want affixes only", info.Lexicon)
	}
	// Affix meanings work before any root has arrived.
	if _, err := a.Affix("rf"); err != nil {
		t.Errorf("Affix before roots: %v", err)
	}

	info, err = a.Load(rootsOnly)
	if err != nil {
		t.Fatalf("Load(roots): %v", err)
	}
	if info.Lexicon.Affixes == 0 {
		t.Error("loading roots dropped the affixes")
	}
	if info.Lexicon.Roots == 0 {
		t.Error("roots did not load")
	}
	if info.Lexicon.Version != doc.Version {
		t.Errorf("Version = %d, want %d", info.Lexicon.Version, doc.Version)
	}
	// The English index is built from roots, so it only works now.
	if got, err := a.Define("water", 0); err != nil || len(got.Senses) == 0 {
		t.Errorf("Define after roots: %v, %d senses", err, len(got.Senses))
	}
}

// TestNeedLexicon pins that the calls needing meanings say so rather
// than answering emptily, which reads as "no such affix".
func TestNeedLexicon(t *testing.T) {
	a := New()
	if _, err := a.Affix("rf"); err != ErrNoLexicon {
		t.Errorf("Affix without a lexicon: %v, want ErrNoLexicon", err)
	}
	if _, err := a.Define("water", 0); err != ErrNoLexicon {
		t.Errorf("Define without a lexicon: %v, want ErrNoLexicon", err)
	}
	// Search still answers from the grammar, which is compiled in.
	if r := a.Search("ERG", SearchOptions{}); len(r.Grammar) == 0 {
		t.Error("Search found no grammar without a lexicon")
	}
}

func TestCompose(t *testing.T) {
	c, err := loaded(t).Compose("S2.CPT-ml-ERG", false)
	if err != nil {
		t.Fatal(err)
	}
	if c.Word == "" || c.Gloss == "" {
		t.Fatalf("Compose = %+v", c)
	}
	// The gloss that comes back is the canonical one for what was
	// built, so a page can show that composing round-tripped.
	if c.Gloss != "S2.CPT-ml-ERG" {
		t.Errorf("Gloss = %q, want the expression back", c.Gloss)
	}
}

func TestCompose_Error(t *testing.T) {
	if _, err := loaded(t).Compose("NOT-A-GLOSS", false); err == nil {
		t.Error("Compose on nonsense reported success")
	}
}

func TestCompare(t *testing.T) {
	c, err := loaded(t).Compare("Maţřëullait", "Malëuţřait")
	if err != nil {
		t.Fatal(err)
	}
	if len(c.Pairs) == 0 {
		t.Fatal("no pairs")
	}
	var differing int
	for _, r := range c.Pairs[0].Slots {
		if r.Differs {
			differing++
		}
	}
	if differing == 0 {
		t.Error("the two canonical words compare as identical")
	}
	if len(c.Pairs[0].Gloss) == 0 {
		t.Error("no glossary differences between words with different affixes")
	}
}

func TestTableAndCategories(t *testing.T) {
	a := New()
	cats := a.Categories()
	if len(cats) == 0 {
		t.Fatal("no categories")
	}
	if all, one := a.Table(""), a.Table("Aspect"); len(one) == 0 || len(one) >= len(all) {
		t.Errorf("Table(Aspect) = %d rows, whole table = %d", len(one), len(all))
	}
	for _, e := range a.Table("Aspect") {
		if e.Abbrev == "" || e.Name == "" {
			t.Errorf("incomplete row: %+v", e)
		}
	}
}

func TestAffix_Degrees(t *testing.T) {
	e, err := loaded(t).Affix("rf")
	if err != nil {
		t.Fatal(err)
	}
	if len(e.Degrees) != 9 {
		t.Errorf("Degrees = %d, want the whole ladder of 9", len(e.Degrees))
	}
	if _, err := loaded(t).Affix("zzzz"); err == nil {
		t.Error("an unknown affix reported success")
	}
}

// TestRoot_StemsAreIndexed pins the one reshaping that is not a
// rename: the internal type spells four stems as four named fields,
// and anything iterating over stems wants an array.
func TestRoot_StemsAreIndexed(t *testing.T) {
	hits := loaded(t).Search("ml", SearchOptions{}).Roots
	if len(hits) == 0 {
		t.Fatal("no root hits for ml")
	}
	for _, h := range hits {
		if len(h.Root.Stems) != 4 {
			t.Fatalf("%s has %d stems, want 4 (0 through 3)", h.Root.Cr, len(h.Root.Stems))
		}
	}
}

// TestReply pins the envelope both arms of the front end depend on.
func TestReply(t *testing.T) {
	var ok struct {
		Ok    *Composed `json:"ok"`
		Error *Error    `json:"error"`
	}
	if err := json.Unmarshal([]byte(Reply(Composed{Word: "wimlo"}, nil)), &ok); err != nil {
		t.Fatal(err)
	}
	if ok.Error != nil || ok.Ok == nil || ok.Ok.Word != "wimlo" {
		t.Errorf("success envelope = %+v", ok)
	}
	if err := json.Unmarshal([]byte(Reply(nil, ErrNoLexicon)), &ok); err != nil {
		t.Fatal(err)
	}
	if ok.Error == nil || ok.Error.Message != ErrNoLexicon.Error() {
		t.Errorf("failure envelope = %+v", ok.Error)
	}
}

// TestInfo_ReportsVersion pins that a page can detect a stale cached
// bundle instead of rendering nonsense against a shape it misreads.
func TestInfo_ReportsVersion(t *testing.T) {
	if got := New().Info().APIVersion; got != APIVersion {
		t.Errorf("APIVersion = %d, want %d", got, APIVersion)
	}
	if l := New().Info().Lexicon; l.Roots != 0 || l.Affixes != 0 {
		t.Errorf("fresh API reports a lexicon: %+v", l)
	}
}

// TestParse_GlossTokensJoin pins the guarantee a page renders against:
// the pieces reproduce the line. Sending both is deliberate. Joining is
// trivial and the gloss syntax is not, so Go decides where every mark
// goes and the front end concatenates.
func TestParse_GlossTokensJoin(t *testing.T) {
	a := loaded(t)
	for _, w := range a.Parse("Maţřëullait wamlaļ hlamröi") {
		if w.Error != "" {
			continue
		}
		var joined string
		for _, tok := range w.GlossTokens {
			joined += tok.Text
		}
		if joined != w.Gloss {
			t.Errorf("%s: tokens join to %q, gloss is %q", w.Romanization, joined, w.Gloss)
		}
	}
	// A word may legitimately have no code at all: "ml-l,/1" is a root
	// and an affix cluster the lexicon does not name, every category at
	// its default and so unwritten. So the claim is about a word that
	// does carry codes, not about every word.
	var codes []string
	for _, tok := range a.Parse("Maţřëullait")[0].GlossTokens {
		if tok.Kind == "code" {
			codes = append(codes, tok.Text)
		}
	}
	if len(codes) < 2 {
		t.Errorf("codes in the canonical word = %v, want the affix abbreviations", codes)
	}
	for _, c := range codes {
		if strings.ContainsAny(c, "-./+_: ()[]{}") {
			t.Errorf("code token %q carries punctuation, so a lookup would miss", c)
		}
	}
}

// TestParse_ViolationsCiteTheRule pins that the browser is told which
// section 2 rule a word breaks, which the CLI and the MCP server both
// report and which this had no way to say.
func TestParse_ViolationsCiteTheRule(t *testing.T) {
	// A legal word breaks nothing.
	if v := New().Parse("Maţřëullait")[0].Violations; len(v) != 0 {
		t.Errorf("the canonical word reports violations: %+v", v)
	}
	var found bool
	for _, w := range New().Parse("tttt xxxx qqqq") {
		for _, v := range w.Violations {
			found = true
			if v.Code == "" || v.Fix == "" {
				t.Errorf("violation with no rule or no fix: %+v", v)
			}
		}
	}
	if !found {
		t.Error("no unpronounceable word in the batch reported a violation")
	}
}

func TestExamplesAndInventory(t *testing.T) {
	a := New()
	ex := a.Examples()
	if len(ex) < 300 {
		t.Errorf("Examples() = %d, want the published corpus", len(ex))
	}
	for _, e := range ex[:5] {
		if e.Ithkuil == "" || e.English == "" {
			t.Errorf("example missing a side: %+v", e)
		}
	}
	inv := a.Inventory()
	if len(inv) < 200 {
		t.Errorf("Inventory() = %d, want one word per grammatical value", len(inv))
	}
	// Every sample must be a word the parser reads back, or it is no
	// use as the far side of a comparison. An unwritten value is the
	// exception and writes nothing at all: the default register is
	// silence, so its sample is the empty string and that is the
	// language, not a failure to build the word.
	var unwritten int
	for _, s := range inv {
		if s.Category == "" || s.Abbrev == "" {
			t.Fatalf("incomplete sample: %+v", s)
		}
		if s.Word == "" {
			if !s.Unwritten && !s.Unmarked {
				t.Errorf("%s/%s renders to nothing but is not marked unwritten", s.Category, s.Abbrev)
			}
			unwritten++
			continue
		}
		if w := a.Parse(s.Word); len(w) == 0 || w[0].Error != "" {
			t.Errorf("%s/%s renders %q, which does not parse back", s.Category, s.Abbrev, s.Word)
		}
	}
	if unwritten == 0 {
		t.Error("no unwritten sample in the set; the flag would be untested")
	}
}

// TestInput_PendingTail pins the half-typed state a text field shows
// dim. Feeding "t" alone leaves it pending, because a "," would still
// turn it into "ţ".
func TestInput_PendingTail(t *testing.T) {
	a := New()
	if got := a.Input("t,rala"); got.Display != "ţrala" {
		t.Errorf("Input(t,rala).Display = %q, want ţrala", got.Display)
	}
	mid := a.Input("mat")
	if mid.Display != "mat" {
		t.Errorf("Input(mat).Display = %q, want mat", mid.Display)
	}
	if mid.Pending == "" {
		t.Error("a trailing t is not pending, so nothing would show dim")
	}
	if mid.Committed+mid.Pending != mid.Display {
		t.Errorf("committed+pending = %q, display = %q", mid.Committed+mid.Pending, mid.Display)
	}
}

// TestParse_BreakdownForEveryClassThatHasOne pins the fix for a real
// hole: the analysis went through view.Segments, which takes a
// Formative, so a modular adjunct and a concatenation chain came back
// with a type and a gloss and an empty table. The CLI has shown both
// all along. Referentials and the rest genuinely have no slot structure
// and are absent here for that reason, not for want of wiring.
func TestParse_BreakdownForEveryClassThatHasOne(t *testing.T) {
	a := loaded(t)
	for _, tc := range []struct{ word, kind string }{
		{"Maţřëullait", "Form"},
		{"ai", "Mod"},
	} {
		w := a.Parse(tc.word)[0]
		if w.Type != tc.kind {
			t.Errorf("%s: type = %q, want %q", tc.word, w.Type, tc.kind)
		}
		if len(w.Segments) == 0 {
			t.Errorf("%s is a %s and has no segment breakdown", tc.word, tc.kind)
		}
		if len(w.Glossary) == 0 {
			t.Errorf("%s has no glossary", tc.word)
		}
	}
}

// TestParse_ChainSplitsIntoMembers pins that a concatenation chain is
// reported per member rather than flattened. Flattening would lose
// which member a slot belongs to, which is the only thing worth knowing
// about a chain.
func TestParse_ChainSplitsIntoMembers(t *testing.T) {
	a := loaded(t)
	// A §3.1.7 chain is written with a hyphen joining its members. The
	// sample has to contain one: with two ordinary words here the
	// search below found nothing, took the skip, and the assertions
	// never ran.
	var chain *Word
	for _, w := range a.Parse("hakšiţé-alcialu'a") {
		if w.Type == "Concat" {
			chain = &w
			break
		}
	}
	if chain == nil {
		t.Fatal("hakšiţé-alcialu'a is a concatenation chain and should classify as one")
	}
	if len(chain.Members) < 2 {
		t.Fatalf("chain has %d members, want at least 2", len(chain.Members))
	}
	if len(chain.Segments) != 0 {
		t.Error("a chain carries flat segments as well as members; only one should be set")
	}
	for _, m := range chain.Members {
		if m.Word == "" || len(m.Segments) == 0 {
			t.Errorf("chain member with no word or no breakdown: %+v", m)
		}
	}
}

// TestParse_UnreadableWordKeepsItsShape pins that a word which fails on
// one slot still comes back with the shape split, which is what makes
// it legible beside a good word instead of a blank row. "espanya" is a
// borrowed name whose "ny" is no Ca complex; everything around it reads
// fine and is worth showing.
//
// A word too broken to split into conjuncts at all gets nothing, and
// that is honest rather than a gap: there is no shape to show. "hwaimļ"
// is one, and the corpus sweep allows for them.
func TestParse_UnreadableWordKeepsItsShape(t *testing.T) {
	w := New().Parse("espanya")[0]
	if w.Error == "" {
		t.Skip("espanya reads now; the corpus drift guard covers the set")
	}
	if len(w.Segments) == 0 {
		t.Error("a word failing on one slot has no shape split, so there is nothing to show")
	}
	if len(w.Violations) != 0 {
		t.Errorf("espanya is pronounceable; violations = %+v", w.Violations)
	}
}

// TestCorpus_BreakdownCoverage is the sweep behind the three tests
// above, and a drift guard in both directions: it fails if a class that
// has a breakdown loses it, and if the count of classes without one
// changes. Referentials, combination referentials, affixual adjuncts
// and carriers have no slot structure in view, and the CLI shows none
// for them either.
func TestCorpus_BreakdownCoverage(t *testing.T) {
	a := New()
	withoutByType := map[string]int{}
	for _, word := range corpus.Words() {
		for _, w := range a.Parse(word) {
			if len(w.Segments) > 0 || len(w.Members) > 0 {
				continue
			}
			kind := w.Type
			if w.Error != "" {
				kind = "(unreadable)"
			}
			withoutByType[kind]++
		}
	}
	for _, kind := range []string{"Form", "Mod", "Concat"} {
		if n := withoutByType[kind]; n != 0 {
			t.Errorf("%d %s words have no breakdown; that class has one", n, kind)
		}
	}
	want := map[string]bool{
		"Ref": true, "CombRef": true, "Affix": true, "Affixes": true,
		"Carrier": true, "(unreadable)": true,
	}
	for kind := range withoutByType {
		if !want[kind] {
			t.Errorf("%s has no breakdown and is not a class known to lack one", kind)
		}
	}
}

// The four calls below had no test at all: the declaration guard
// covered their shapes and only a smoke test outside the suite had ever
// run them.

func TestPositions(t *testing.T) {
	ps := New().Positions()
	if len(ps) < 10 {
		t.Fatalf("Positions() = %d, want one per slot", len(ps))
	}
	a := New()
	for _, p := range ps {
		if p.Slot == "" || p.Name == "" {
			t.Errorf("position with no slot or name: %+v", p)
		}
		// Every category named must be one Table answers to, or a
		// builder populating a control from it gets nothing.
		for _, c := range p.Categories {
			if len(a.Table(c)) == 0 {
				t.Errorf("%s names category %q, which Table does not know", p.Slot, c)
			}
		}
	}
}

func TestTopicsAndNote(t *testing.T) {
	a := loaded(t)
	tops := a.Topics()
	if len(tops) < 20 {
		t.Fatalf("Topics() = %d, want the explanations belonging to no value", len(tops))
	}
	for _, top := range tops[:5] {
		if top.Key == "" || top.Category == "" {
			t.Errorf("topic with no key or category: %+v", top)
		}
	}
	// A grammar value with a note.
	got, err := a.Note("DPX")
	if err != nil {
		t.Fatalf("Note(DPX): %v", err)
	}
	if got.Guidance == "" || got.Explanation == "" {
		t.Errorf("Note(DPX) has no authored text: %+v", got)
	}
	// A value with no note is not an error: most have none.
	if _, err := a.Note("THM"); err != nil {
		t.Errorf("Note(THM): %v, want the entry with empty notes", err)
	}
	// A topic is reachable by its own key.
	if _, err := a.Note(tops[0].Key); err != nil {
		t.Errorf("Note(%s): %v, want the topic", tops[0].Key, err)
	}
	if _, err := a.Note("NOTATHING"); err == nil {
		t.Error("Note on an unknown code reported success")
	}
}

func TestFromASCII(t *testing.T) {
	a := New()
	if got := a.FromASCII("t,rala"); got != "ţrala" {
		t.Errorf("FromASCII(t,rala) = %q, want ţrala", got)
	}
	if got := a.FromASCII("mlala"); got != "mlala" {
		t.Errorf("FromASCII left plain text alone as %q", got)
	}
}

// TestSearchOptions_Filters pins the four narrowings the CLI and the
// MCP server expose. They lived in those two front ends separately
// until this package grew them, which is why they are checked here now
// rather than twice.
func TestSearchOptions_Filters(t *testing.T) {
	a := loaded(t)

	// Category lists a whole category and ignores the query.
	byCat := a.Search("", SearchOptions{Category: "Aspect"})
	if len(byCat.Grammar) < 30 {
		t.Errorf("Category=Aspect gave %d rows, want the whole category", len(byCat.Grammar))
	}
	for _, e := range byCat.Grammar {
		if e.Category != "Aspect" {
			t.Errorf("%s/%s survived an Aspect filter", e.Category, e.Abbrev)
		}
	}
	// A category listing is a grammar request with no lexicon half.
	if len(byCat.Roots) != 0 || len(byCat.Affixes) != 0 {
		t.Error("a category listing answered with lexicon hits")
	}

	// Exact requires the abbreviation, not a substring of a description.
	exact := a.Search("DPX", SearchOptions{Exact: true})
	if len(exact.Grammar) != 1 || exact.Grammar[0].Abbrev != "DPX" {
		t.Errorf("Exact gave %+v, want just DPX", exact.Grammar)
	}

	// Form reads the query as written letters and answers from the
	// grammar alone: what a root contains is a different question.
	form := a.Search("ëu", SearchOptions{Form: true})
	if len(form.Grammar) == 0 {
		t.Error("ëu is a written form and encodes something")
	}
	if len(form.Roots) != 0 {
		t.Error("a written form is not a lexicon question")
	}
	// Form and Category compose: "what does this write, among Biases".
	if got := a.Search("a", SearchOptions{Form: true, Category: "Bias"}); len(got.Grammar) != 0 {
		t.Errorf("a is not a bias form; got %+v", got.Grammar)
	}

	// Limit caps each lexicon kind. A negative limit uncaps it.
	if got := a.Search("water", SearchOptions{Limit: 3}); len(got.Roots) > 3 {
		t.Errorf("Limit=3 gave %d roots", len(got.Roots))
	}
	capped := a.Search("water", SearchOptions{})
	uncapped := a.Search("water", SearchOptions{Limit: -1})
	if len(uncapped.Roots) < len(capped.Roots) {
		t.Error("a negative limit returned fewer hits than the default cap")
	}
}

// TestAffixesAndRoots_Paging pins the browsing endpoints. A search box
// cannot find what you do not know the name of, so the affix ladder and
// the lexicon have to be walkable as well as searchable.
func TestAffixesAndRoots_Paging(t *testing.T) {
	a := loaded(t)

	all := a.Affixes(0, 0)
	if all.Total != 528 || len(all.Items) != 528 {
		t.Errorf("Affixes(0,0) = %d of %d, want all 528", len(all.Items), all.Total)
	}
	// Ordered by cluster, so a page is stable between calls.
	for i := 1; i < len(all.Items); i++ {
		if all.Items[i-1].Cs >= all.Items[i].Cs {
			t.Fatalf("affixes out of order at %d: %q then %q",
				i, all.Items[i-1].Cs, all.Items[i].Cs)
		}
	}
	page := a.Affixes(10, 5)
	if len(page.Items) != 5 || page.Offset != 10 || page.Total != 528 {
		t.Errorf("Affixes(10,5) = %d items, offset %d, total %d", len(page.Items), page.Offset, page.Total)
	}
	if page.Items[0].Cs != all.Items[10].Cs {
		t.Errorf("page starts at %q, whole list has %q at 10", page.Items[0].Cs, all.Items[10].Cs)
	}

	roots := a.Roots(0, 4)
	if roots.Total < 5000 || len(roots.Items) != 4 {
		t.Errorf("Roots(0,4) = %d items of %d", len(roots.Items), roots.Total)
	}
	for _, r := range roots.Items {
		if r.Cr == "" || len(r.Stems) != 4 {
			t.Errorf("root without a cluster or four stems: %+v", r)
		}
	}
	// Stepping past the end finds the end, rather than an error.
	if end := a.Roots(999999, 10); len(end.Items) != 0 || end.Total != roots.Total {
		t.Errorf("past the end = %d items, total %d", len(end.Items), end.Total)
	}
	// No lexicon is an empty page, not a panic.
	if got := New().Affixes(0, 10); got.Total != 0 || len(got.Items) != 0 {
		t.Errorf("Affixes with no lexicon = %+v", got)
	}
}

// TestCompose_Stressless pins the §4.8 alternative: stress written as a
// parsing adjunct instead of a diacritic, for a reader or a font that
// cannot show one.
func TestCompose_Stressless(t *testing.T) {
	a := loaded(t)
	plain, err := a.Compose("S2.CPT-ml-ERG", false)
	if err != nil {
		t.Fatal(err)
	}
	adjunct, err := a.Compose("S2.CPT-ml-ERG", true)
	if err != nil {
		t.Fatal(err)
	}
	if plain.Word == adjunct.Word {
		t.Errorf("stressless wrote the same word: %q", plain.Word)
	}
	if !strings.Contains(adjunct.Word, " ") {
		t.Errorf("stressless = %q, want a parsing adjunct before the word", adjunct.Word)
	}
	if plain.Gloss != adjunct.Gloss {
		t.Errorf("the same grammar glossed two ways: %q and %q", plain.Gloss, adjunct.Gloss)
	}
}

// TestCompose_Chain pins that a chain gloss composes. Reading the whole
// expression as one word takes the space between two formatives for
// part of a root, which fails with a confusing complaint about the Cr.
func TestCompose_Chain(t *testing.T) {
	a := loaded(t)
	one, err := a.Compose("S2.CPT-ml-ERG", false)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := a.Compose(one.Gloss+" "+one.Gloss, false); err == nil {
		t.Error("two separate words composed as one")
	}
}

// TestDefine_LimitAndMore pins that a capped list says how much it left
// out, rather than implying it is all there is.
func TestDefine_LimitAndMore(t *testing.T) {
	a := loaded(t)
	all, err := a.Define("water", -1)
	if err != nil {
		t.Fatal(err)
	}
	if len(all.Senses) < 3 {
		t.Skipf("water has %d senses; too few to test a cap", len(all.Senses))
	}
	capped, err := a.Define("water", 2)
	if err != nil {
		t.Fatal(err)
	}
	if len(capped.Senses) != 2 {
		t.Errorf("limit 2 gave %d senses", len(capped.Senses))
	}
	if capped.More != len(all.Senses)-2 {
		t.Errorf("More = %d, want %d", capped.More, len(all.Senses)-2)
	}
	if all.More != 0 {
		t.Errorf("an uncapped list reports %d more", all.More)
	}
	for _, s := range capped.Senses {
		if s.Word == "" || s.Gloss == "" || s.Meaning == "" {
			t.Errorf("sense missing a field: %+v", s)
		}
	}
}

// TestSetNotes pins the store-backed path to the authored text. A
// caller reading the lexicon from SQLite gets the notes this way; the
// browser gets them in the JSON it fetches. Both must end up the same.
func TestSetNotes(t *testing.T) {
	a := New()
	a.SetNotes([]GrammarEntry{{
		Abbrev: "DPX", Explanation: "two-halved", Guidance: "a pair of X",
	}}, []Topic{{Key: "frame", Category: "Case-Frame"}})
	got, err := a.Note("DPX")
	if err != nil {
		t.Fatal(err)
	}
	if got.Explanation != "two-halved" || got.Guidance != "a pair of X" {
		t.Errorf("Note(DPX) = %+v", got)
	}
	if len(a.Topics()) != 1 {
		t.Errorf("Topics() = %d, want the one that was set", len(a.Topics()))
	}
	if a.Info().Lexicon.Explained != 1 {
		t.Errorf("Explained = %d, want 1", a.Info().Lexicon.Explained)
	}
}
