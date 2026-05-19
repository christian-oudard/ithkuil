package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/coudard/ithkuil/go/tokenize"
)

// cmdDiff renders a slot-by-slot diff between two formatives or two
// aligned sentences. Single-word pair: `--diff A B`. Sentence pair:
// `--diff WORDS... -- WORDS...`. For sentences, words are paired
// positionally; any extras on either side are listed at the end.
func cmdDiff(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, diffUsage)
		return 2
	}
	lhsRaw, rhsRaw := splitDiffArgs(args)
	lhs := tokenize.Tokenize(strings.Join(lhsRaw, " "))
	rhs := tokenize.Tokenize(strings.Join(rhsRaw, " "))
	if len(lhs) == 0 || len(rhs) == 0 {
		fmt.Fprintln(stderr, diffUsage)
		return 2
	}
	renderDiff(stdout, lhs, rhs)
	return 0
}

const diffUsage = `usage: ithkuil-gloss --diff WORD_A WORD_B
   or: ithkuil-gloss --diff WORDS... -- WORDS...`

// splitDiffArgs splits at the first "--", or treats exactly 2 args as
// a single-word pair when no separator is given.
func splitDiffArgs(args []string) ([]string, []string) {
	for i, a := range args {
		if a == "--" {
			return args[:i], args[i+1:]
		}
	}
	if len(args) == 2 {
		return args[:1], args[1:]
	}
	return args, nil
}

func renderDiff(stdout io.Writer, lhs, rhs []tokenize.WordToken) {
	n := len(lhs)
	if len(rhs) < n {
		n = len(rhs)
	}
	multi := len(lhs) > 1 || len(rhs) > 1
	for i := 0; i < n; i++ {
		if multi {
			fmt.Fprintf(stdout, "[%d]\n", i+1)
		}
		showWordDiff(stdout, lhs[i], rhs[i])
		if multi {
			fmt.Fprintln(stdout)
		}
	}
	for i := n; i < len(lhs); i++ {
		fmt.Fprintf(stdout, "[%d] A-only: %s\n", i+1, lhs[i].Surface())
	}
	for i := n; i < len(rhs); i++ {
		fmt.Fprintf(stdout, "[%d] B-only: %s\n", i+1, rhs[i].Surface())
	}
}

type diffRow struct {
	label string
	a, b  string
}

func diffRows(a, b tokenize.WordToken) []diffRow {
	return []diffRow{
		{"type", traceType(a), traceType(b)},
		{"Slot I  (Cc)", traceSlotI(a), traceSlotI(b)},
		{"Slot II (Vv)", traceSlotII(a), traceSlotII(b)},
		{"Slot III(Cr)", traceSlotIII(a), traceSlotIII(b)},
		{"Slot IV (Vr)", traceSlotIV(a), traceSlotIV(b)},
		{"Slot V+VII afx", traceSlotV(a), traceSlotV(b)},
		{"Slot VI (Ca)", traceSlotVI(a), traceSlotVI(b)},
		{"Slot VIII", traceSlotVIII(a), traceSlotVIII(b)},
		{"Slot IX (Vc/k)", traceSlotIX(a), traceSlotIX(b)},
		{"stress", traceStress(a), traceStress(b)},
	}
}

func showWordDiff(stdout io.Writer, a, b tokenize.WordToken) {
	rows := diffRows(a, b)
	const labelW = 18
	colW := 14
	if n := len([]rune(a.Surface())) + 2; n > colW {
		colW = n
	}

	// Header.
	fmt.Fprintf(stdout, "%-*s%-*s  %s\n", labelW, "", colW, a.Surface(), b.Surface())
	dividerW := labelW + colW + 2 + len([]rune(b.Surface()))
	fmt.Fprintln(stdout, strings.Repeat("─", dividerW))

	changes := 0
	for _, r := range rows {
		marker := "  "
		if r.a != r.b {
			marker = "→ "
			changes++
		}
		fmt.Fprintf(stdout, "%-*s%-*s%s%s\n", labelW, r.label, colW, r.a, marker, r.b)
	}

	if changes == 0 {
		fmt.Fprintln(stdout, "  no slot changes")
	} else {
		fmt.Fprintf(stdout, "  %d of %d rows changed\n", changes, len(rows))
	}
}
