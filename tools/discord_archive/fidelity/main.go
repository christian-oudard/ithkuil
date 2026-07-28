// Measures what we make of attested community usage.
//
// Two separate questions, which earlier versions of this tool ran
// together and got wrong. Coverage asks how much of the corpus we
// understand at all; fidelity asks how faithfully we handle the part
// that is a formative.
//
// Coverage goes through tokenize.ClassifyWord, the same entry point the
// CLI and the MCP server use. Calling fullparse.Formative directly
// instead counted every referential, bias adjunct and affixual adjunct
// in the corpus as a parse failure — 372 of them, over half of what
// looked like a 696-word gap. A referential is not a broken formative.
//
// Fidelity applies only to formatives, since they are the only token
// kind with a renderer. Three questions, in order of importance:
//
//  1. Is the round-trip lossless? parse -> render -> parse must land on
//     the same gloss. A failure here is a real bug.
//  2. Is what we emit a legal word by our own phonotactics? A failure
//     is a real bug too, and one a round-trip alone cannot see: a form
//     both halves mishandle the same way still comes back equal.
//  3. Does the canonical spelling match what a human wrote? A mismatch
//     is a style choice, not necessarily a defect, because the renderer
//     canonicalizes and the grammar permits several spellings.
//
// Whatever is left unclassified is the triage list, printed by the
// formative-parse error that best explains the shape.
package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"sort"
	"strings"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/surface"
	"github.com/christian-oudard/ithkuil/tokenize"
	"github.com/christian-oudard/ithkuil/validation"
	"github.com/christian-oudard/ithkuil/view"
)

func main() {
	f, err := os.Open(os.Args[1])
	if err != nil {
		panic(err)
	}
	defer f.Close()

	gl := &gloss.Glosser{Canonical: true}
	var total, formatives, same, lossless, invalid int
	var lossy, illegal []string
	byKind := map[string]int{}
	triage := newTriage()

	sc := bufio.NewScanner(f)
	for sc.Scan() {
		w := strings.TrimSpace(sc.Text())
		if w == "" {
			continue
		}
		total++

		tok := tokenize.ClassifyWord(w)
		kind := view.Type(tok)
		byKind[kind]++

		fs, ok := formativesOf(tok)
		if !ok {
			if kind == "?" {
				triage.add(w)
			}
			continue
		}
		formatives++

		out := renderAll(fs)
		before := glossAll(gl, fs)
		if out == surface.Normalize(w) {
			same++
		}
		// Is what we emit even a legal word?
		if r := validation.ValidateWord(out); !r.Valid {
			invalid++
			if len(illegal) < 10 {
				illegal = append(illegal, fmt.Sprintf("%s -> %s", w, out))
			}
		}
		// Does it survive a second trip?
		again, ok := formativesOf(tokenize.ClassifyWord(out))
		switch {
		case !ok:
			lossy = append(lossy, fmt.Sprintf("%s -> %s (does not re-parse)", w, out))
		case glossAll(gl, again) == before:
			lossless++
		default:
			lossy = append(lossy, fmt.Sprintf("%s -> %s (%s vs %s)",
				w, out, before, glossAll(gl, again)))
		}
	}

	understood := total - byKind["?"]
	fmt.Printf("candidate words:      %d\n", total)
	fmt.Printf("understood:           %d (%.1f%%)\n", understood, pct(understood, total))
	fmt.Println()
	fmt.Println("by token type:")
	for _, k := range sortedByCount(byKind) {
		fmt.Printf("  %-9s %6d\n", k, byKind[k])
	}
	fmt.Println()
	fmt.Printf("formative fidelity (of %d):\n", formatives)
	fmt.Printf("  round-trip lossless:    %d (%.1f%%)\n", lossless, pct(lossless, formatives))
	fmt.Printf("  output is a legal word: %d (%.1f%%)\n", formatives-invalid, pct(formatives-invalid, formatives))
	fmt.Printf("  spelled as attested:    %d (%.1f%%)\n", same, pct(same, formatives))

	report("lossy round-trips", lossy)
	report("phonotactically invalid output", illegal)
	triage.report()
}

// formativesOf pulls the formatives out of a token, for the two kinds
// that hold them. Everything else classifies fine but has no renderer,
// so it counts toward coverage and sits out the fidelity checks.
func formativesOf(t tokenize.WordToken) ([]g.Formative, bool) {
	switch v := t.(type) {
	case tokenize.FormativeWord:
		return []g.Formative{v.Formative}, true
	case tokenize.ConcatenatedFormativeWord:
		return v.Chain.Formatives(), true
	}
	return nil, false
}

// A §3.1.7 chain is written as one word, so its links rejoin on the
// hyphen after being rendered or glossed individually.
func renderAll(fs []g.Formative) string {
	out := make([]string, len(fs))
	for i, f := range fs {
		out[i] = render.Formative(f)
	}
	return strings.Join(out, "-")
}

func glossAll(gl *gloss.Glosser, fs []g.Formative) string {
	out := make([]string, len(fs))
	for i, f := range fs {
		out[i] = gl.Formative(f)
	}
	return strings.Join(out, "-")
}

// Quoted material in an error is the offending morpheme, which is what
// makes two reports of the same defect look different. Blanking it
// buckets them together.
var quoted = regexp.MustCompile(`"[^"]*"`)

// triageList groups unclassified words by why they failed to parse as a
// formative. Not every one of them is a formative, but the formative
// decoder gets furthest into the word of any of the classifiers, so its
// complaint is the most specific description of the shape available.
type triageList struct {
	counts  map[string]int
	samples map[string][]string
}

func newTriage() *triageList {
	return &triageList{counts: map[string]int{}, samples: map[string][]string{}}
}

func (t *triageList) add(word string) {
	reason := "no error (rejected by every classifier)"
	for _, link := range strings.Split(word, "-") {
		if _, err := fullparse.Formative(link); err != nil {
			reason = quoted.ReplaceAllString(err.Error(), `"X"`)
			break
		}
	}
	t.counts[reason]++
	if len(t.samples[reason]) < 4 {
		t.samples[reason] = append(t.samples[reason], word)
	}
}

func (t *triageList) report() {
	if len(t.counts) == 0 {
		return
	}
	fmt.Printf("\nunclassified, by formative-parse error:\n")
	for _, r := range sortedByCount(t.counts) {
		fmt.Printf("  %5d  %-52s %v\n", t.counts[r], r, t.samples[r])
	}
}

// sortedByCount orders keys by descending count, ties broken by name so
// two runs over the same corpus print identically.
func sortedByCount(m map[string]int) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		if m[keys[i]] != m[keys[j]] {
			return m[keys[i]] > m[keys[j]]
		}
		return keys[i] < keys[j]
	})
	return keys
}

func report(label string, xs []string) {
	if len(xs) == 0 {
		return
	}
	fmt.Printf("\n%s:\n", label)
	sort.Strings(xs)
	for _, x := range xs {
		fmt.Println(" ", x)
	}
}

func pct(a, b int) float64 {
	if b == 0 {
		return 0
	}
	return 100 * float64(a) / float64(b)
}
