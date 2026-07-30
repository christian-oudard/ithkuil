package main

import (
	"io"
	"os"
)

// Color scheme: phonetic chunks cyan, slot labels yellow, grammar
// codes magenta, canonical names bold, romanization headings bold,
// everything explanatory (meanings, table headers, the "(modular
// adjunct)" hint) dim. Disable with NO_COLOR=1 or when stdout is not
// a terminal.

const (
	ansiReset   = "\033[0m"
	ansiBold    = "\033[1m"
	ansiDim     = "\033[2m"
	ansiCyan    = "\033[36m"
	ansiYellow  = "\033[33m"
	ansiMagenta = "\033[35m"
	ansiGreen   = "\033[32m"
	// Red marks the one thing that is wrong, and nothing else. It is
	// the only colour here that carries a judgment rather than a
	// category, so spending it on anything routine would blunt it.
	ansiRed = "\033[31m"
)

// colorsOn is initialised from the writer and environment. analyze
// sets this before rendering; render helpers read it via stylize().
var colorsOn = false

// setColorMode decides whether colors should be emitted to w. Off
// when NO_COLOR is set, when --color=never, or when w isn't a
// terminal (e.g. piped to a file).
func setColorMode(w io.Writer, force string) {
	switch force {
	case "always":
		colorsOn = true
		return
	case "never":
		colorsOn = false
		return
	}
	if _, ok := os.LookupEnv("NO_COLOR"); ok {
		colorsOn = false
		return
	}
	colorsOn = isTerminalWriter(w)
}

func isTerminalWriter(w io.Writer) bool {
	f, ok := w.(*os.File)
	if !ok {
		return false
	}
	fi, err := f.Stat()
	if err != nil {
		return false
	}
	return (fi.Mode() & os.ModeCharDevice) != 0
}

// stylize wraps text in an ANSI style sequence when colors are on.
func stylize(style, text string) string {
	if !colorsOn || text == "" {
		return text
	}
	return style + text + ansiReset
}
