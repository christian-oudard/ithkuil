// Package inspect extracts compact, per-slot string representations of
// formative tokens and renders them as a polygraph (trace) or a
// side-by-side diff.
//
// Both the ithkuil-gloss CLI and the ithkuil-mcp server depend on
// these helpers, so the per-slot extractors are public.
package inspect

import (
	"fmt"
	"io"
	"strings"

	g "github.com/christian-oudard/ithkuil/grammar"
	"github.com/christian-oudard/ithkuil/tokenize"
)

// Dot is the placeholder rendered when a slot is at its grammatical
// default. Using a single visible character keeps the trace table
// aligned and makes monotone rows easy to scan.
const Dot = "·"

// Row pairs a slot label with one per-token cell. Used by Polygraph.
type Row struct {
	Label string
	Cells []string
}

// DiffRow pairs a slot label with the two values being compared.
type DiffRow struct {
	Label string
	A, B  string
}

// rowSpec holds the label and per-slot extractor used by both
// Polygraph and DiffRows so the two views stay in lock-step.
type rowSpec struct {
	label   string
	extract func(tokenize.WordToken) string
}

var rowSpecs = []rowSpec{
	{"type", Type},
	{"Slot I  (Cc)", SlotI},
	{"Slot II (Vv)", SlotII},
	{"Slot III(Cr)", SlotIII},
	{"Slot IV (Vr)", SlotIV},
	{"Slot V+VII afx", SlotV},
	{"Slot VI (Ca)", SlotVI},
	{"Slot VIII", SlotVIII},
	{"Slot IX (Vc/k)", SlotIX},
	{"stress", Stress},
}

// TraceRows returns one row per slot spec with cells extracted for
// each token.
func TraceRows(tokens []tokenize.WordToken) []Row {
	out := make([]Row, len(rowSpecs))
	for i, spec := range rowSpecs {
		cells := make([]string, len(tokens))
		for j, t := range tokens {
			cells[j] = spec.extract(t)
		}
		out[i] = Row{Label: spec.label, Cells: cells}
	}
	return out
}

// DiffRows returns one diff entry per slot spec for the two tokens.
func DiffRows(a, b tokenize.WordToken) []DiffRow {
	out := make([]DiffRow, len(rowSpecs))
	for i, spec := range rowSpecs {
		out[i] = DiffRow{Label: spec.label, A: spec.extract(a), B: spec.extract(b)}
	}
	return out
}

// Polygraph renders a per-slot table: rows are slots, columns are
// tokens, with a stats footer summarizing how many slots are
// exercised, monotone, or unused across real-formative columns.
func Polygraph(w io.Writer, tokens []tokenize.WordToken) {
	if len(tokens) == 0 {
		return
	}
	rows := TraceRows(tokens)

	colW := 10
	for _, t := range tokens {
		if n := len([]rune(t.Surface())); n+2 > colW {
			colW = n + 2
		}
	}
	const labelW = 18

	fmt.Fprintf(w, "%-*s", labelW, "")
	for _, t := range tokens {
		fmt.Fprintf(w, "%-*s", colW, t.Surface())
	}
	fmt.Fprintln(w)
	fmt.Fprintln(w, strings.Repeat("─", labelW+colW*len(tokens)))

	for _, r := range rows {
		fmt.Fprintf(w, "%-*s", labelW, r.Label)
		for _, c := range r.Cells {
			fmt.Fprintf(w, "%-*s", colW, c)
		}
		fmt.Fprintln(w)
	}

	// Stats footer. Type row + Slot I row are header/setup; the real
	// formative slots are rows[2:].
	formativeRows := rows[2:]
	real := realColumns(tokens)
	pickReal := func(cells []string) []string {
		out := make([]string, 0, len(cells))
		for i, c := range cells {
			if real[i] {
				out = append(out, c)
			}
		}
		return out
	}

	defaultRows, monoRows := 0, 0
	for _, r := range formativeRows {
		cs := pickReal(r.Cells)
		switch {
		case allDefault(cs):
			defaultRows++
		case monotone(cs):
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

	fmt.Fprintln(w)
	if n <= 1 {
		fmt.Fprintf(w, "  slots filled: %d / unused: %d (of %d; single-formative sentence, no monotony)\n",
			total-defaultRows, defaultRows, total)
	} else {
		exercised := total - defaultRows - monoRows
		fmt.Fprintf(w, "  slots exercised: %d / monotone: %d / unused: %d (of %d, across %d formatives)\n",
			exercised, monoRows, defaultRows, total, n)
	}
}

// realColumns flags which token positions count as "real" formatives
// for stats purposes: a formative word that isn't the foreign-text
// payload sitting immediately after a carrier word.
func realColumns(tokens []tokenize.WordToken) []bool {
	out := make([]bool, len(tokens))
	for i, t := range tokens {
		if _, ok := t.(tokenize.FormativeWord); !ok {
			continue
		}
		if i > 0 {
			if _, prevCarrier := tokens[i-1].(tokenize.CarrierWord); prevCarrier {
				continue
			}
		}
		out[i] = true
	}
	return out
}

func allDefault(cs []string) bool {
	for _, c := range cs {
		if c != Dot {
			return false
		}
	}
	return true
}

func monotone(cs []string) bool {
	var nonDot string
	for _, c := range cs {
		if c == Dot {
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

// Diff writes a side-by-side slot diff for paired tokens. Extra
// tokens on either side are listed at the end as A-only / B-only.
func Diff(w io.Writer, lhs, rhs []tokenize.WordToken) {
	n := len(lhs)
	if len(rhs) < n {
		n = len(rhs)
	}
	multi := len(lhs) > 1 || len(rhs) > 1
	for i := 0; i < n; i++ {
		if multi {
			fmt.Fprintf(w, "[%d]\n", i+1)
		}
		writeWordDiff(w, lhs[i], rhs[i])
		if multi {
			fmt.Fprintln(w)
		}
	}
	for i := n; i < len(lhs); i++ {
		fmt.Fprintf(w, "[%d] A-only: %s\n", i+1, lhs[i].Surface())
	}
	for i := n; i < len(rhs); i++ {
		fmt.Fprintf(w, "[%d] B-only: %s\n", i+1, rhs[i].Surface())
	}
}

func writeWordDiff(w io.Writer, a, b tokenize.WordToken) {
	rows := DiffRows(a, b)
	const labelW = 18
	colW := 14
	if n := len([]rune(a.Surface())) + 2; n > colW {
		colW = n
	}

	fmt.Fprintf(w, "%-*s%-*s  %s\n", labelW, "", colW, a.Surface(), b.Surface())
	dividerW := labelW + colW + 2 + len([]rune(b.Surface()))
	fmt.Fprintln(w, strings.Repeat("─", dividerW))

	changes := 0
	for _, r := range rows {
		marker := "  "
		if r.A != r.B {
			marker = "→ "
			changes++
		}
		fmt.Fprintf(w, "%-*s%-*s%s%s\n", labelW, r.Label, colW, r.A, marker, r.B)
	}

	if changes == 0 {
		fmt.Fprintln(w, "  no slot changes")
	} else {
		fmt.Fprintf(w, "  %d of %d rows changed\n", changes, len(rows))
	}
}

// Type returns a short tag identifying the token kind: Form, Concat,
// Ref, CombRef, Bias, Reg, Mod, Carrier, (fgn), or "?" for unknown.
func Type(t tokenize.WordToken) string {
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
	case tokenize.SingleAffixWord:
		return "Affix"
	case tokenize.MultipleAffixWord:
		return "Affixes"
	case tokenize.CarrierWord:
		return "Carrier"
	case tokenize.ForeignWord:
		return "(fgn)"
	case tokenize.UnknownWord:
		return "?"
	}
	return "?"
}

// SlotI returns "T1"/"T2" for concatenated formatives, Dot otherwise.
func SlotI(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok || f.Formative.Concat == nil {
		return Dot
	}
	switch *f.Formative.Concat {
	case g.Type1:
		return "T1"
	case g.Type2:
		return "T2"
	}
	return Dot
}

// SlotII returns "Stem/Version" for formatives, Dot otherwise. Stem
// is implicit (S1) for CsRoot and RefRoot; we still show it.
func SlotII(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	switch r := f.Formative.Root.(type) {
	case g.CrRoot:
		return fmt.Sprintf("%s/%s", r.Stem, r.Version)
	case g.CsRoot:
		return fmt.Sprintf("S1/%s", r.Version)
	case g.RefRoot:
		return fmt.Sprintf("S1/%s", r.Version)
	}
	return Dot
}

// SlotIII returns the root identifier (Cr cluster, Cs identifier, or
// referential C1) for formatives, or the joined referent list for
// referentials.
func SlotIII(t tokenize.WordToken) string {
	switch v := t.(type) {
	case tokenize.FormativeWord:
		switch r := v.Formative.Root.(type) {
		case g.CrRoot:
			if r.Cluster == "" {
				return Dot
			}
			return r.Cluster
		case g.CsRoot:
			return r.Cs
		case g.RefRoot:
			return r.C1
		}
	case tokenize.ReferentialWord:
		parts := make([]string, len(v.Refs))
		for i, r := range v.Refs {
			parts[i] = r.Referent.String()
		}
		return strings.Join(parts, "+")
	}
	return Dot
}

// SlotIV returns the non-default Function/Specification/Context
// values joined by "/", or Dot if everything is at the default.
func SlotIV(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	var fn g.Function
	var sp g.Specification
	var ctx g.Context
	switch r := f.Formative.Root.(type) {
	case g.CrRoot:
		fn, sp, ctx = r.SlotIV.Function, r.SlotIV.Specification, r.SlotIV.Context
	case g.CsRoot:
		fn, sp, ctx = r.Function, g.BSC, r.Context
	case g.RefRoot:
		fn, sp, ctx = r.SlotIV.Function, r.SlotIV.Specification, r.SlotIV.Context
	default:
		return Dot
	}
	var parts []string
	if fn != g.STA {
		parts = append(parts, fn.String())
	}
	if sp != g.BSC {
		parts = append(parts, sp.String())
	}
	if ctx != g.EXS {
		parts = append(parts, ctx.String())
	}
	if len(parts) == 0 {
		return Dot
	}
	return strings.Join(parts, "/")
}

// SlotV returns "N+M" — counts of Slot V and Slot VII affixes — or
// Dot if there are no affixes.
func SlotV(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	total := len(f.Formative.SlotV) + len(f.Formative.SlotVII)
	if total == 0 {
		return Dot
	}
	return fmt.Sprintf("%d+%d", len(f.Formative.SlotV), len(f.Formative.SlotVII))
}

// SlotVI returns the non-default Ca features joined by "/", or Dot.
func SlotVI(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	s := f.Formative.SlotVI
	if s == g.DefaultSlotVI {
		return Dot
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

// SlotVIII returns the VnCn content rendered as "Vn.MoodScope", or
// Dot when the slot is empty. The MoodScope label is the verbal
// Mood (FAC/SUB/…) when the formative is verbal, the nominal
// CaseScope (CCN/CCA/…) otherwise.
func SlotVIII(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok || f.Formative.SlotVIII == nil {
		return Dot
	}
	label := g.SlotVIIIVnLabel(f.Formative.SlotVIII)
	if label == "" {
		return Dot
	}
	return label + "." + moodOrScopeLabel(f.Formative)
}

// moodOrScopeLabel renders the Slot VIII Mood field as either a Mood
// label (when the formative is verbal) or its CaseScope twin (when
// nominal/framed). The mapping mirrors the Mood / CaseScope ordinal
// correspondence in the spec.
func moodOrScopeLabel(f g.Formative) string {
	mood := g.SlotVIIIMoodScope(f.SlotVIII)
	if g.IsVerbal(f.Final) {
		return mood.String()
	}
	return g.MoodToCaseScope(mood).String()
}

// SlotIX returns the case (Vc) or illocution/validation (Vk) encoded
// by the Final, with THM and bare ASR/OBS suppressed as defaults.
func SlotIX(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	switch v := f.Formative.Final.(type) {
	case g.UnframedNominal:
		if v.Case == g.THM {
			return Dot
		}
		return v.Case.String()
	case g.FramedVerbal:
		if v.Case == g.THM {
			return Dot
		}
		return v.Case.String()
	case g.UnframedVerbal:
		return vkLabel(v.Vk)
	}
	return Dot
}

// vkLabel formats a Vk variant as "ILL" or "ILL/VAL" — non-default
// Validation is shown only on Assertive (the only Vk that carries
// one).
func vkLabel(vk g.Vk) string {
	if asr, ok := vk.(g.Assertive); ok {
		if asr.Validation == g.OBS {
			return "ASR"
		}
		return "ASR/" + asr.Validation.String()
	}
	return vk.Tag()
}

// Stress returns the Final tag (ANT for framed-verbal, ULT for
// verbal, Dot for the nominal/penultimate default).
func Stress(t tokenize.WordToken) string {
	f, ok := t.(tokenize.FormativeWord)
	if !ok {
		return Dot
	}
	if tag := f.Formative.Final.Tag(); tag != "" {
		return tag
	}
	return Dot
}
