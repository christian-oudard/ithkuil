// Command ithkuil-mcp is a Model Context Protocol server that exposes
// the Ithkuil V4 grammar tooling as MCP tools and resources over
// stdio.
//
// Usage: ithkuil-mcp [-lex DIR] [-grammar DIR]
//
//	-lex DIR       Override the embedded lexicon with one read from DIR
//	               (expects roots.json and affixes.json). Loaded once at
//	               startup.
//	-grammar DIR   Directory holding grammar reference markdown
//	               (default ./grammar_reference). Files are served as
//	               MCP resources.
//
// Run as a subprocess of any MCP client (Claude Desktop, Claude Code,
// etc.) — it speaks stdio JSON-RPC and exits when the client closes.
package main

import (
	"context"
	"flag"
	"log"
	"os"
	"path/filepath"

	"github.com/modelcontextprotocol/go-sdk/mcp"

	"github.com/christian-oudard/ithkuil/lexicon"
)

// server bundles the per-process state every tool handler needs.
type server struct {
	lex        *lexicon.Lexicon
	grammarDir string
}

func main() {
	lexDir := flag.String("lex", "", "override the embedded lexicon with one read from DIR")
	grammarDir := flag.String("grammar", "./grammar_reference", "directory with grammar reference markdown")
	flag.Parse()

	var lex *lexicon.Lexicon
	var err error
	if *lexDir == "" {
		lex, err = lexicon.LoadDefault()
	} else {
		lex, err = lexicon.Load(
			filepath.Join(*lexDir, "roots.json"),
			filepath.Join(*lexDir, "affixes.json"),
		)
	}
	if err != nil {
		log.Printf("warning: lexicon load failed (%v); roots/affixes lookups will return empty", err)
		lex = &lexicon.Lexicon{}
	}

	s := &server{lex: lex, grammarDir: *grammarDir}

	mcpServer := mcp.NewServer(
		&mcp.Implementation{Name: "ithkuil", Version: "v0.1.0"},
		nil,
	)
	s.registerTools(mcpServer)
	s.registerResources(mcpServer)

	if err := mcpServer.Run(context.Background(), &mcp.StdioTransport{}); err != nil {
		// stdio EOF means the client disconnected. Other errors are real.
		if err != context.Canceled {
			log.Fatalf("mcp server: %v", err)
		}
	}
	_ = os.Stdout.Sync()
}
