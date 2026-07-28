package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/modelcontextprotocol/go-sdk/mcp"
)

// grammarResource pairs a public URI with the markdown file (relative
// to s.grammarDir) it serves.
type grammarResource struct {
	URI         string
	File        string
	Title       string
	Description string
}

var grammarResources = []grammarResource{
	{
		URI:         "grammar://morphology",
		File:        "morphology.md",
		Title:       "Ithkuil V4 Morphology",
		Description: "Canonical reference for phonology, slot structure, cases, adjuncts, syntax, and numbers.",
	},
	{
		URI:         "grammar://affixes",
		File:        "affixes_reference.md",
		Title:       "Ithkuil V4 Affixes Reference",
		Description: "All 527 affixes with gradient types and 9 degrees.",
	},
	{
		URI:         "grammar://phonotactics",
		File:        "phonotactics.md",
		Title:       "Ithkuil V4 Phonotactics",
		Description: "Detailed consonant cluster rules, the ones the parse tool checks a word against.",
	},
}

func (s *server) registerResources(srv *mcp.Server) {
	for _, r := range grammarResources {
		r := r // capture
		srv.AddResource(&mcp.Resource{
			URI:         r.URI,
			Name:        r.Title,
			Title:       r.Title,
			Description: r.Description,
			MIMEType:    "text/markdown",
		}, func(_ context.Context, req *mcp.ReadResourceRequest) (*mcp.ReadResourceResult, error) {
			return s.readGrammarResource(r, req.Params.URI)
		})
	}
}

func (s *server) readGrammarResource(r grammarResource, uri string) (*mcp.ReadResourceResult, error) {
	path := filepath.Join(s.grammarDir, r.File)
	body, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", r.File, err)
	}
	return &mcp.ReadResourceResult{
		Contents: []*mcp.ResourceContents{{
			URI:      uri,
			MIMEType: "text/markdown",
			Text:     string(body),
		}},
	}, nil
}
