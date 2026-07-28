// Command ithkuil-input is a small TUI that maps ASCII keystrokes
// to the Ithkuil Unicode orthography in real time. Pending
// keystrokes (vowel runs that may still extend, or starter
// characters awaiting a second char to form a digraph) are shown
// dimmed; committed text is bright.
//
// When stdin is not a terminal, the program degrades to batch
// mode: read ASCII, write Unicode.
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"

	"golang.org/x/term"

	"github.com/christian-oudard/ithkuil/surface"
)

const (
	ansiDim    = "\x1b[2m"
	ansiReset  = "\x1b[0m"
	ansiClear  = "\x1b[K"
	prompt     = "> "
	keyCtrlC   = 3
	keyCtrlD   = 4
	keyEnter   = '\r'
	keyNewline = '\n'
	keyBackOne = 127
	keyBackTwo = 8
	keyEscape  = 27
)

func main() {
	fd := int(os.Stdin.Fd())
	if !term.IsTerminal(fd) {
		batch(os.Stdin, os.Stdout)
		return
	}
	old, err := term.MakeRaw(fd)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	defer term.Restore(fd, old)
	if err := tui(os.Stdin, os.Stdout); err != nil && err != io.EOF {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

// batch reads ASCII from r and writes the FromASCII conversion to w
// with no TUI niceties. Suitable for shell pipes.
func batch(r io.Reader, w io.Writer) {
	b, err := io.ReadAll(r)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	fmt.Fprint(w, surface.FromASCII(string(b)))
}

// tui runs the interactive raw-mode loop. Each Enter finalizes the
// current line into permanent output; Ctrl+C or Ctrl+D on an empty
// buffer exits.
func tui(in io.Reader, out io.Writer) error {
	r := bufio.NewReader(in)
	var s surface.InputState
	draw(out, &s)
	for {
		c, _, err := r.ReadRune()
		if err != nil {
			fmt.Fprint(out, "\r\n")
			return err
		}
		switch c {
		case keyCtrlC:
			fmt.Fprint(out, "\r\n")
			return nil
		case keyCtrlD:
			if s.Display() == "" {
				fmt.Fprint(out, "\r\n")
				return nil
			}
		case keyEnter, keyNewline:
			word := s.Commit()
			// Repaint the line as final (no dim) then move on.
			fmt.Fprintf(out, "\r%s%s%s\r\n", ansiClear, prompt, word)
			s.Reset()
		case keyBackOne, keyBackTwo:
			s.Backspace()
		case keyEscape:
			consumeEscape(r)
		default:
			s.Feed(c)
		}
		draw(out, &s)
	}
}

// draw repaints the current line: prompt + committed (bright) +
// pending (dim).
func draw(out io.Writer, s *surface.InputState) {
	fmt.Fprintf(out, "\r%s%s%s%s%s%s",
		ansiClear, prompt, s.Committed(), ansiDim, s.Pending(), ansiReset)
}

// consumeEscape swallows the rest of a CSI/ESC sequence (e.g. arrow
// keys) so it doesn't get fed into the input method as digraph
// characters.
func consumeEscape(r *bufio.Reader) {
	b, err := r.Peek(1)
	if err != nil || len(b) == 0 {
		return
	}
	if b[0] != '[' && b[0] != 'O' {
		return
	}
	r.ReadByte() // consume '[' or 'O'
	for {
		c, err := r.ReadByte()
		if err != nil {
			return
		}
		// CSI sequences end with a byte in 0x40..0x7e.
		if c >= 0x40 && c <= 0x7e {
			return
		}
	}
}
