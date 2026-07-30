package main

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/modelcontextprotocol/go-sdk/mcp"
)

// grammarDir returns the reference directory as seen from this package,
// which is where the server's default -grammar flag points when the
// command is run from the repo root.
func grammarDir() string { return filepath.Join("..", "..", "..", "docs", "reference") }

// TestGrammarResources_AllServable is the check the resource list needs
// most: every URI it advertises must have a file behind it. A renamed
// or moved markdown file is otherwise only discovered by a client
// asking for it.
func TestGrammarResources_AllServable(t *testing.T) {
	s := &server{grammarDir: grammarDir()}
	for _, r := range grammarResources {
		res, err := s.readGrammarResource(r, r.URI)
		if err != nil {
			t.Errorf("%s: %v", r.URI, err)
			continue
		}
		if len(res.Contents) != 1 {
			t.Errorf("%s: %d contents, want 1", r.URI, len(res.Contents))
			continue
		}
		c := res.Contents[0]
		if c.URI != r.URI {
			t.Errorf("%s: served under %q", r.URI, c.URI)
		}
		if c.MIMEType != "text/markdown" {
			t.Errorf("%s: mime %q", r.URI, c.MIMEType)
		}
		if len(c.Text) < 1000 {
			t.Errorf("%s: %d bytes, too short to be the reference", r.URI, len(c.Text))
		}
	}
	if len(grammarResources) == 0 {
		t.Fatal("no resources declared")
	}
}

func TestReadGrammarResource_Missing(t *testing.T) {
	s := &server{grammarDir: t.TempDir()}
	_, err := s.readGrammarResource(grammarResources[0], grammarResources[0].URI)
	if err == nil {
		t.Fatal("a missing file is an error")
	}
	if !strings.Contains(err.Error(), grammarResources[0].File) {
		t.Errorf("error %q does not name the file", err)
	}
}

// TestRegister covers the two registration functions by handing them a
// real server and then asking it what it has. Registration is where a
// tool gets its name and schema, so a tool that fails to register is
// invisible to every client and to every other test in this file, which
// call the handlers directly.
func TestRegister(t *testing.T) {
	s := &server{grammarDir: grammarDir()}
	srv := mcp.NewServer(&mcp.Implementation{Name: "ithkuil-test", Version: "test"}, nil)
	s.registerTools(srv)
	s.registerResources(srv)

	ctx := context.Background()
	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "test"}, nil)
	ct, st := mcp.NewInMemoryTransports()
	serverSession, err := srv.Connect(ctx, st, nil)
	if err != nil {
		t.Fatalf("server connect: %v", err)
	}
	defer serverSession.Close()
	session, err := client.Connect(ctx, ct, nil)
	if err != nil {
		t.Fatalf("client connect: %v", err)
	}
	defer session.Close()

	tools, err := session.ListTools(ctx, nil)
	if err != nil {
		t.Fatalf("list tools: %v", err)
	}
	got := map[string]bool{}
	for _, tool := range tools.Tools {
		got[tool.Name] = true
		if tool.Description == "" {
			t.Errorf("tool %q has no description; a client shows it to choose by", tool.Name)
		}
	}
	for _, want := range []string{"parse", "compare", "compose", "search", "define"} {
		if !got[want] {
			t.Errorf("tool %q not registered; have %v", want, got)
		}
	}

	resources, err := session.ListResources(ctx, nil)
	if err != nil {
		t.Fatalf("list resources: %v", err)
	}
	if len(resources.Resources) != len(grammarResources) {
		t.Errorf("registered %d resources, declared %d",
			len(resources.Resources), len(grammarResources))
	}
}

// TestRun is the end-to-end path: a real client over an in-memory
// transport, calling a tool by name with JSON arguments, which is the
// only way the argument decoding and the result encoding get exercised
// at all.
func TestRun(t *testing.T) {
	s := testServer(t)
	srv := mcp.NewServer(&mcp.Implementation{Name: "ithkuil-test", Version: "test"}, nil)
	s.registerTools(srv)

	ctx := context.Background()
	ct, st := mcp.NewInMemoryTransports()
	serverSession, err := srv.Connect(ctx, st, nil)
	if err != nil {
		t.Fatalf("server connect: %v", err)
	}
	defer serverSession.Close()
	session, err := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "test"}, nil).
		Connect(ctx, ct, nil)
	if err != nil {
		t.Fatalf("client connect: %v", err)
	}
	defer session.Close()

	res, err := session.CallTool(ctx, &mcp.CallToolParams{
		Name:      "parse",
		Arguments: map[string]any{"text": "malëuţřait"},
	})
	if err != nil {
		t.Fatalf("call parse: %v", err)
	}
	if res.IsError {
		t.Fatalf("parse returned an error result: %+v", res.Content)
	}
	out, ok := res.StructuredContent.(map[string]any)
	if !ok {
		t.Fatalf("structured content is %T, want an object", res.StructuredContent)
	}
	words, ok := out["words"].([]any)
	if !ok || len(words) != 1 {
		t.Fatalf("words = %v", out["words"])
	}

	// And an error from a handler must reach the client as a tool error
	// rather than as a transport failure.
	res, err = session.CallTool(ctx, &mcp.CallToolParams{
		Name:      "parse",
		Arguments: map[string]any{"text": ""},
	})
	if err != nil {
		t.Fatalf("call parse with empty text: %v", err)
	}
	if !res.IsError {
		t.Error("empty text should come back as a tool error")
	}
}

// TestGrammarDirDefault guards the default the command ships with. It
// is a relative path, so it only resolves when the server is started
// from the repo root, and that is worth stating: a client configured
// with a different working directory gets no resources.
func TestGrammarDirDefault(t *testing.T) {
	for _, r := range grammarResources {
		if _, err := os.Stat(filepath.Join(grammarDir(), r.File)); err != nil {
			t.Errorf("%s is declared but not in docs/reference: %v", r.File, err)
		}
	}
}
