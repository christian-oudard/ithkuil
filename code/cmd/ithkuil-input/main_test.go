package main

import (
	"bytes"
	"errors"
	"io"
	"strings"
	"testing"
)

// TestTUIEnterCommits feeds an ASCII line followed by Enter and
// Ctrl+D, then checks that the committed word appears in the
// output stream.
func TestTUIEnterCommits(t *testing.T) {
	in := strings.NewReader("Mat,rqeeullait\r\x04")
	var out bytes.Buffer
	err := tui(in, &out)
	if err != nil && err != io.EOF {
		t.Fatalf("tui returned %v", err)
	}
	if !strings.Contains(out.String(), "Maţřëullait") {
		t.Errorf("output missing committed word: %q", out.String())
	}
}

// TestTUIBackspace verifies the BS key removes one displayed char.
func TestTUIBackspace(t *testing.T) {
	// "aa" → pending "ä". One BS pops the raw 'a' (pending "a"),
	// a second BS empties it. Enter then commits an empty line.
	in := strings.NewReader("aa\x7f\x7f\r\x04")
	var out bytes.Buffer
	err := tui(in, &out)
	if err != nil && err != io.EOF {
		t.Fatalf("tui returned %v", err)
	}
	// The Enter writes "\r\x1b[K> <word>\r\n". After two
	// backspaces <word> is empty.
	if !strings.Contains(out.String(), "> \r\n") {
		t.Errorf("expected empty committed line, got %q", out.String())
	}
}

// TestTUIEscapeIgnored ensures that arrow-key escape sequences are
// swallowed instead of being fed as starter keystrokes.
func TestTUIEscapeIgnored(t *testing.T) {
	// Up arrow is ESC [ A.
	in := strings.NewReader("t,\x1b[A\r\x04")
	var out bytes.Buffer
	err := tui(in, &out)
	if err != nil && err != io.EOF {
		t.Fatalf("tui returned %v", err)
	}
	if !strings.Contains(out.String(), "ţ") {
		t.Errorf("expected ţ in output, got %q", out.String())
	}
}

// TestBatch is the non-terminal path, which is what a shell pipe gets
// and the only way the command is scriptable. It is FromASCII and
// nothing else: no prompt, no dimming, no trailing newline added.
func TestBatch(t *testing.T) {
	var out bytes.Buffer
	if err := batch(strings.NewReader("Mat,rqeeullait\n"), &out); err != nil {
		t.Fatalf("batch: %v", err)
	}
	if got, want := out.String(), "Maţřëullait\n"; got != want {
		t.Errorf("batch = %q, want %q", got, want)
	}
}

func TestBatch_Empty(t *testing.T) {
	var out bytes.Buffer
	if err := batch(strings.NewReader(""), &out); err != nil {
		t.Fatalf("batch: %v", err)
	}
	if out.Len() != 0 {
		t.Errorf("batch of nothing wrote %q", out.String())
	}
}

// TestTUICtrlC covers the other way out. Ctrl+C leaves regardless of
// what is in the buffer, where Ctrl+D only leaves an empty one, so a
// half-typed word is not a trap.
func TestTUICtrlC(t *testing.T) {
	var out bytes.Buffer
	if err := tui(strings.NewReader("mal\x03"), &out); err != nil {
		t.Fatalf("Ctrl+C should be a clean exit, got %v", err)
	}
	if !strings.HasSuffix(out.String(), "\r\n") {
		t.Errorf("exit should end the line; got %q", out.String())
	}
}

// TestTUICtrlDWithText is the asymmetry worth pinning: Ctrl+D on a
// non-empty buffer is ignored, so it cannot discard typing. Here it is
// followed by Enter, which commits, and only then does a second Ctrl+D
// on the now-empty buffer leave.
func TestTUICtrlDWithText(t *testing.T) {
	var out bytes.Buffer
	if err := tui(strings.NewReader("mal\x04\r\x04"), &out); err != nil && err != io.EOF {
		t.Fatalf("tui returned %v", err)
	}
	if !strings.Contains(out.String(), "> mal\r\n") {
		t.Errorf("the word survived Ctrl+D and should have committed; got %q", out.String())
	}
}

// TestTUIEOF covers the read-error return: input that just stops,
// which is what a closed pipe looks like.
func TestTUIEOF(t *testing.T) {
	var out bytes.Buffer
	err := tui(strings.NewReader("mal"), &out)
	if err != io.EOF {
		t.Errorf("tui at end of input = %v, want io.EOF", err)
	}
	if !strings.HasSuffix(out.String(), "\r\n") {
		t.Errorf("the line should be ended; got %q", out.String())
	}
}

// TestConsumeEscape covers the three shapes an ESC can take. A bare
// ESC at end of input and an ESC followed by an ordinary character are
// both left alone: only a real CSI or SS3 introducer starts a sequence
// worth swallowing, and treating every ESC as one would eat the next
// keystroke.
func TestConsumeEscape(t *testing.T) {
	cases := []struct {
		name  string
		in    string
		wants string
	}{
		{"csi arrow", "t,\x1b[A\r\x04", "ţ"},
		{"ss3 arrow", "t,\x1bOA\r\x04", "ţ"},
		{"bare escape at end", "t,\x1b", "ţ"},
		{"escape then letter", "t,\x1bm\r\x04", "ţ"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			var out bytes.Buffer
			if err := tui(strings.NewReader(c.in), &out); err != nil && err != io.EOF {
				t.Fatalf("tui returned %v", err)
			}
			if !strings.Contains(out.String(), c.wants) {
				t.Errorf("output %q missing %q", out.String(), c.wants)
			}
		})
	}
}

// errReader fails on the first read, standing in for a pipe that
// breaks mid-stream.
type errReader struct{ err error }

func (e errReader) Read([]byte) (int, error) { return 0, e.err }

func TestBatch_ReadError(t *testing.T) {
	want := errors.New("pipe broke")
	var out bytes.Buffer
	if err := batch(errReader{want}, &out); !errors.Is(err, want) {
		t.Errorf("batch error = %v, want %v", err, want)
	}
}

// TestConsumeEscape_Truncated covers the loop's error return: a CSI
// introducer with no final byte, which is what a sequence cut off by
// the end of input looks like.
func TestConsumeEscape_Truncated(t *testing.T) {
	var out bytes.Buffer
	if err := tui(strings.NewReader("t,\x1b[1;2"), &out); err != io.EOF {
		t.Errorf("tui = %v, want io.EOF", err)
	}
	if !strings.Contains(out.String(), "ţ") {
		t.Errorf("the text before the escape should survive; got %q", out.String())
	}
}
