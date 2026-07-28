package main

import (
	"bytes"
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
