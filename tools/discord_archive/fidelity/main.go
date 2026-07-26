// Measures our canonical spelling against attested community usage.
// Two questions, in order of importance:
//
//  1. Is the round-trip lossless? parse -> render -> parse must land on
//     the same gloss. A failure here is a real bug.
//  2. Does the canonical spelling match what a human wrote? A mismatch
//     is a style choice, not necessarily a defect.
package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/gloss"
	"github.com/christian-oudard/ithkuil/render"
	"github.com/christian-oudard/ithkuil/surface"
	"github.com/christian-oudard/ithkuil/validation"
)

func main() {
	f, err := os.Open(os.Args[1])
	if err != nil {
		panic(err)
	}
	defer f.Close()

	gl := &gloss.Glosser{Canonical: true}
	var total, parsed, same, lossless, invalid int
	var lossy, illegal []string

	sc := bufio.NewScanner(f)
	for sc.Scan() {
		w := strings.TrimSpace(sc.Text())
		if w == "" {
			continue
		}
		total++
		out, before, ok := glossAndRender(gl, w)
		if !ok {
			continue
		}
		parsed++
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
		_, after, ok := glossAndRender(gl, out)
		switch {
		case !ok:
			lossy = append(lossy, fmt.Sprintf("%s -> %s (does not re-parse)", w, out))
		case before == after:
			lossless++
		default:
			lossy = append(lossy, fmt.Sprintf("%s -> %s (%s vs %s)", w, out, before, after))
		}
	}

	fmt.Printf("candidate words:      %d\n", total)
	fmt.Printf("parsed as formative:  %d (%.1f%%)\n", parsed, pct(parsed, total))
	fmt.Printf("round-trip lossless:  %d (%.1f%% of parsed)\n", lossless, pct(lossless, parsed))
	fmt.Printf("output is a legal word: %d (%.1f%% of parsed)\n", parsed-invalid, pct(parsed-invalid, parsed))
	fmt.Printf("spelled as attested:  %d (%.1f%% of parsed)\n", same, pct(same, parsed))
	report("lossy round-trips", lossy)
	report("phonotactically invalid output", illegal)
}

// glossAndRender parses one word and returns its canonical spelling and
// its gloss. A hyphen joins a §3.1.7 chain of formatives, so each link
// is handled on its own and the results rejoined.
func glossAndRender(gl *gloss.Glosser, word string) (out, gloss string, ok bool) {
	var outs, glosses []string
	for _, link := range strings.Split(word, "-") {
		f, err := fullparse.Formative(link)
		if err != nil {
			return "", "", false
		}
		outs = append(outs, render.Formative(f))
		glosses = append(glosses, gl.Formative(f))
	}
	return strings.Join(outs, "-"), strings.Join(glosses, "-"), true
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
