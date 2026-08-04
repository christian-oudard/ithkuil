package main

import (
	"context"
	"testing"
)

// The MCP SDK turns a returned Go error into a protocol error whose
// body is one string, so a tool that reports a bad input that way
// hands an AI client prose and nothing else. A gloss the caller got
// wrong is not a protocol failure: the tool did its job and the
// answer is that the expression does not name a word. That answer
// belongs in the result, where its stage and code survive.
//
// A missing argument stays an error, because that is the caller
// misusing the tool rather than the tool reporting on the input.

func TestMCPCompose_MissingArgumentIsAnError(t *testing.T) {
	s := testServer(t)
	if _, _, err := s.compose(context.Background(), nil, composeIn{Expression: "  "}); err == nil {
		t.Error("want an error naming the missing argument")
	}
}

func TestMCPCompose_FaultsReachTheCallerAsData(t *testing.T) {
	s := testServer(t)
	_, out, err := s.compose(context.Background(), nil,
		composeIn{Expression: "S9-ZZZ-ml-QQQ"})
	if err != nil {
		t.Fatalf("compose returned a protocol error for a bad gloss: %v", err)
	}
	if len(out.Faults) != 3 {
		t.Fatalf("faults = %+v, want one per bad token", out.Faults)
	}
	for _, f := range out.Faults {
		if f.Stage == "" || f.Code == "" || f.Found == "" || f.Fix == "" {
			t.Errorf("incomplete fault: %+v", f)
		}
	}
	if out.Error == "" {
		t.Error("the message a client prints is gone")
	}
	if out.Romanization != "" {
		t.Errorf("a gloss that did not read still produced %q", out.Romanization)
	}
}

// Compare splits the same way compose does: a missing argument is the
// caller misusing the tool, an unreadable word is the tool's answer.
func TestMCPCompare_AnUnreadableWordIsAResult(t *testing.T) {
	s := testServer(t)
	_, out, err := s.compare(context.Background(), nil, compareIn{A: "akxq", B: "mala"})
	if err != nil {
		t.Fatalf("compare returned a protocol error for a bad word: %v", err)
	}
	if len(out.Faults) == 0 {
		t.Fatal("no faults in the result")
	}
	f := out.Faults[0]
	if f.Stage == "" || f.Code == "" || f.Fix == "" {
		t.Errorf("incomplete fault: %+v", f)
	}
}
