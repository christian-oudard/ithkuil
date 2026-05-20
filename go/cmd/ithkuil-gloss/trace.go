package main

import (
	"fmt"
	"io"
	"strings"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/tokenize"
)

const traceDot = "·"

// cmdTrace renders a per-slot polygraph: each row is a formative slot,
// each column is a token. Defaults show as "·" so monotone or unused
// dimensions stand out visually. Footer summarizes how many slots are
// exercised, monotone, or unused.
func cmdTrace(args []string, stdout, stderr io.Writer, lexDir string) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil-gloss --trace WORD1 [WORD2 ...]")
		return 2
	}
	tokens := tokenize.Tokenize(strings.Join(args, " "))
	if len(tokens) == 0 {
		return 0
	}

	type row struct {
		label string
		cells []string
	}
	rows := []row{
		{"type", traceTypeRow(tokens)},
		{"Slot I  (Cc)", traceCol(tokens, traceSlotI)},
		{"Slot II (Vv)", traceCol(tokens, traceSlotII)},
		{"Slot III(Cr)", traceCol(tokens, traceSlotIII)},
		{"Slot IV (Vr)", traceCol(tokens, traceSlotIV)},
		{"Slot V+VII afx", traceCol(tokens, traceSlotV)},
		{"Slot VI (Ca)", traceCol(tokens, traceSlotVI)},
		{"Slot VIII", traceCol(tokens, traceSlotVIII)},
		{"Slot IX (Vc/k)", traceCol(tokens, traceSlotIX)},
		{"stress", traceCol(tokens, traceStress)},
	}

	colW := 10
	for _, t := range tokens {
		if n := len([]rune(t.Surface())); n+2 > colW {
			colW = n + 2
		}
	}
	const labelW = 18

	// Header.
	fmt.Fprintf(stdout, "%-*s", labelW, "")
	for _, t := range tokens {
		fmt.Fprintf(stdout, "%-*s", colW, t.Surface())
	}
	fmt.Fprintln(stdout)
	fmt.Fprintln(stdout, strings.Repeat("─", labelW+colW*len(tokens)))

	// Rows.
	for _, r := range rows {
		fmt.Fprintf(stdout, "%-*s", labelW, r.label)
		for _, c := range r.cells {
			fmt.Fprintf(stdout, "%-*s", colW, c)
		}
		fmt.Fprintln(stdout)
	}

	// Stats footer.
	formativeRows := rows[2:]
	// Real-formative columns: not the foreign-text immediately after a
	// carrier word.
	real := make([]bool, len(tokens))
	for i, t := range tokens {
		if _, ok := t.(tokenize.FormativeWord); !ok {
			continue
		}
		if i > 0 {
			if _, prevCarrier := tokens[i-1].(tokenize.CarrierWord); prevCarrier {
				continue
			}
		}
		real[i] = true
	}
	pickReal := func(cells []string) []string {
		out := make([]string, 0, len(cells))
		for i, c := range cells {
			if real[i] {
				out = append(out, c)
			}
		}
		return out
	}
	isAllDefault := func(cs []string) bool {
		for _, c := range cs {
			if c != traceDot {
				return false
			}
		}
		return true
	}
	isMonotone := func(cs []string) bool {
		var nonDot string
		for _, c := range cs {
			if c == traceDot {
				continue
			}
			if nonDot == "" {
				nonDot = c
			} else if c != nonDot {
				return false
			}
		}
		return true
	}

	defaultRows, monoRows := 0, 0
	for _, r := range formativeRows {
		cs := pickReal(r.cells)
		switch {
		case isAllDefault(cs):
			defaultRows++
		case isMonotone(cs):
			monoRows++
		}
	}
	total := len(formativeRows)
	n := 0
	for _, b := range real {
		if b {
			n++
		}
	}

	fmt.Fprintln(stdout)
	if n <= 1 {
		fmt.Fprintf(stdout, "  slots filled: %d / unused: %d (of %d; single-formative sentence, no monotony)\n",
			total-defaultRows, defaultRows, total)
	} else {
		exercised := total - defaultRows - monoRows
		fmt.Fprintf(stdout, "  slots exercised: %d / monotone: %d / unused: %d (of %d, across %d formatives)\n",
			exercised, monoRows, defaultRows, total, n)
	}
	return 0
}

func traceCol(tokens []tokenize.WordToken, f func(tokenize.WordToken) string) []string {
	out := make([]string, len(tokens))
	for i, t := range tokens {
		out[i] = f(t)
	}
	return out
}

func traceTypeRow(tokens []tokenize.WordToken) []string {
	out := make([]string, len(tokens))
	for i, t := range tokens {
		out[i] = traceType(t)
	}
	return out
}

func traceType(t tokenize.WordToken) string {
	switch t.(type) {
	case tokenize.FormativeWord:
		return "Form"
	case tokenize.ConcatenatedFormativeWord:
		return "Concat"
	case tokenize.ReferentialWord:
		return "Ref"
	case tokenize.CombinationRefWord:
		return "CombRef"
	case tokenize.BiasWord:
		return "Bias"
	case tokenize.RegisterStartWord, tokenize.RegisterEndWord:
		return "Reg"
	case tokenize.ModularWord:
		return "Mod"
	case tokenize.CarrierWord:
		return "Carrier"
	case tokenize.ForeignWord:
		return "(fgn)"
	case tokenize.UnknownWord:
		return "?"
	}
	return "?"
}

func traceSlotI(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok || f.Formative.SlotI == nil {
		return traceDot
	}
	switch *f.Formative.SlotI {
	case g.Type1:
		return "T1"
	case g.Type2:
		return "T2"
	}
	return traceDot
}

func traceSlotII(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	s := f.Formative.SlotII
	return fmt.Sprintf("%s/%s", s.Stem, s.Version)
}

func traceSlotIII(t tokenize.WordToken) string {
	switch v := t.(type) {
	case tokenize.FormativeWord:
		if v.Formative.SlotIII == "" {
			return traceDot
		}
		return string(v.Formative.SlotIII)
	case tokenize.ReferentialWord:
		parts := make([]string, len(v.Refs))
		for i, r := range v.Refs {
			parts[i] = r.Referent.String()
		}
		return strings.Join(parts, "+")
	}
	return traceDot
}

func traceSlotIV(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	s := f.Formative.SlotIV
	var parts []string
	if s.Function != g.STA {
		parts = append(parts, s.Function.String())
	}
	if s.Specification != g.BSC {
		parts = append(parts, s.Specification.String())
	}
	if s.Context != g.EXS {
		parts = append(parts, s.Context.String())
	}
	if len(parts) == 0 {
		return traceDot
	}
	return strings.Join(parts, "/")
}

func traceSlotV(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	total := len(f.Formative.SlotV) + len(f.Formative.SlotVII)
	if total == 0 {
		return traceDot
	}
	return fmt.Sprintf("%d+%d", len(f.Formative.SlotV), len(f.Formative.SlotVII))
}

func traceSlotVI(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	s := f.Formative.SlotVI
	if s == g.DefaultSlotVI {
		return traceDot
	}
	var parts []string
	if s.Configuration != g.UNI {
		parts = append(parts, s.Configuration.String())
	}
	if s.Affiliation != g.CSL {
		parts = append(parts, s.Affiliation.String())
	}
	if s.Perspective != g.M_ {
		parts = append(parts, s.Perspective.String())
	}
	if s.Extension != g.DEL {
		parts = append(parts, s.Extension.String())
	}
	if s.Essence != g.NRM {
		parts = append(parts, s.Essence.String())
	}
	return strings.Join(parts, "/")
}

func traceSlotVIII(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok || f.Formative.SlotVIII == nil {
		return traceDot
	}
	switch v := f.Formative.SlotVIII.(type) {
	case g.VnCnAspect:
		return v.Aspect.String() + "." + moodOrScopeShort(v.MS)
	case g.VnCnValence:
		return v.Valence.String() + "." + moodOrScopeShort(v.MS)
	case g.VnCnPhase:
		return v.Phase.String() + "." + moodOrScopeShort(v.MS)
	case g.VnCnEffect:
		return v.Effect.String() + "." + moodOrScopeShort(v.MS)
	case g.VnCnLevel:
		return v.Level.String() + "." + moodOrScopeShort(v.MS)
	}
	return traceDot
}

func moodOrScopeShort(ms g.MoodOrScope) string {
	switch v := ms.(type) {
	case g.MoodVal:
		return v.Mood.String()
	case g.CaseScopeVal:
		return v.CaseScope.String()
	}
	return ""
}

func traceSlotIX(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	if f.Formative.SlotIX == nil {
		return traceDot
	}
	switch v := f.Formative.SlotIX.(type) {
	case g.CaseSlot:
		if v.Case == g.THM {
			return traceDot
		}
		return v.Case.String()
	case g.Assertive:
		if v.Validation == g.OBS {
			return "ASR"
		}
		return "ASR/" + v.Validation.String()
	}
	return f.Formative.SlotIX.Tag()
}

func traceStress(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	if f.Formative.Stress == g.Penultimate {
		return traceDot
	}
	return f.Formative.Stress.String()
}
