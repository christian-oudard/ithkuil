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
	if !ok || f.Formative.Concat == nil {
		return traceDot
	}
	switch *f.Formative.Concat {
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
	switch r := f.Formative.Root.(type) {
	case g.CrRoot:
		return fmt.Sprintf("%s/%s", r.Stem, r.Version)
	case g.CsRoot:
		return fmt.Sprintf("Cs/%s", r.Version)
	case g.RefRoot:
		return fmt.Sprintf("Ref/%s", r.Version)
	}
	return traceDot
}

func traceSlotIII(t tokenize.WordToken) string {
	switch v := t.(type) {
	case tokenize.FormativeWord:
		switch r := v.Formative.Root.(type) {
		case g.CrRoot:
			if r.Cluster == "" {
				return traceDot
			}
			return r.Cluster
		case g.CsRoot:
			return r.Cs
		case g.RefRoot:
			return r.C1
		}
		return traceDot
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
	var parts []string
	switch r := f.Formative.Root.(type) {
	case g.CrRoot:
		if r.SlotIV.Function != g.STA {
			parts = append(parts, r.SlotIV.Function.String())
		}
		if r.SlotIV.Specification != g.BSC {
			parts = append(parts, r.SlotIV.Specification.String())
		}
		if r.SlotIV.Context != g.EXS {
			parts = append(parts, r.SlotIV.Context.String())
		}
	case g.CsRoot:
		parts = append(parts, fmt.Sprintf("D%d", r.Degree))
		if r.Function != g.STA {
			parts = append(parts, r.Function.String())
		}
		if r.Context != g.EXS {
			parts = append(parts, r.Context.String())
		}
	case g.RefRoot:
		if r.SlotIV.Function != g.STA {
			parts = append(parts, r.SlotIV.Function.String())
		}
		if r.SlotIV.Specification != g.BSC {
			parts = append(parts, r.SlotIV.Specification.String())
		}
		if r.SlotIV.Context != g.EXS {
			parts = append(parts, r.SlotIV.Context.String())
		}
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
	verbal := traceFinalIsVerbal(f.Formative.Final)
	switch v := f.Formative.SlotVIII.(type) {
	case g.VnCnAspect:
		return v.Aspect.String() + "." + moodScopeLabel(v.MoodScope, verbal)
	case g.VnCnValence:
		return v.Valence.String() + "." + moodScopeLabel(v.MoodScope, verbal)
	case g.VnCnPhase:
		return v.Phase.String() + "." + moodScopeLabel(v.MoodScope, verbal)
	case g.VnCnEffect:
		return v.Effect.String() + "." + moodScopeLabel(v.MoodScope, verbal)
	case g.VnCnLevel:
		return v.Level.String() + "." + moodScopeLabel(v.MoodScope, verbal)
	}
	return traceDot
}

func traceFinalIsVerbal(f g.Final) bool {
	switch f.(type) {
	case g.UnframedVerbal, g.FramedVerbal:
		return true
	}
	return false
}

func moodScopeLabel(m g.Mood, verbal bool) string {
	if verbal {
		return m.String()
	}
	return g.MoodToCaseScope(m).String()
}

func traceSlotIX(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	switch v := f.Formative.Final.(type) {
	case nil:
		return traceDot
	case g.UnframedNominal:
		if v.Case == g.THM {
			return traceDot
		}
		return v.Case.String()
	case g.FramedVerbal:
		if v.Case == g.THM {
			return traceDot
		}
		return v.Case.String()
	case g.UnframedVerbal:
		if as, ok := v.Vk.(g.Assertive); ok {
			if as.Validation == g.OBS {
				return "ASR"
			}
			return "ASR/" + as.Validation.String()
		}
		return v.Vk.Tag()
	}
	return traceDot
}

func traceStress(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return traceDot
	}
	if f.Formative.Final == nil {
		return traceDot
	}
	tag := f.Formative.Final.Tag()
	if tag == "" {
		return traceDot
	}
	return tag
}
