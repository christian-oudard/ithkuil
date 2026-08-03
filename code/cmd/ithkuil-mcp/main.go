// Command ithkuil-mcp is a Model Context Protocol server that exposes
// the Ithkuil V4 grammar tooling as MCP tools and resources over
// stdio.
//
// Usage: ithkuil-mcp [-data FILE] [-grammar DIR]
//
//	-data FILE     Path to data.db (default: $XDG_DATA_HOME/ithkuil/data.db).
//	-grammar DIR   Directory holding grammar reference markdown
//	               (default ./docs/reference). Files are served as
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

	"github.com/modelcontextprotocol/go-sdk/mcp"

	"github.com/christian-oudard/ithkuil/lexicon"
	"github.com/christian-oudard/ithkuil/store"
)

// server bundles the per-process state every tool handler needs.
type server struct {
	lex        *lexicon.Lexicon
	st         *store.Store
	grammarDir string
}

// newServer builds what main runs. A store that will not open is a
// warning rather than an exit: the grammar tables are compiled into the
// binary, so parse, compose, compare and grammar search all still
// answer, and only the lexicon-backed half comes back empty. A client
// launches this as a subprocess and has nowhere to show a startup
// failure, so refusing to start would look like the server is broken
// rather than like the store is missing.
func newServer(dataFile, grammarDir string) *server {
	st, err := store.Open(dataFile)
	if err != nil {
		log.Printf("warning: cannot open data store %s (%v); roots/affixes lookups will return empty", dataFile, err)
	}

	lex := &lexicon.Lexicon{}
	if st != nil {
		lex, err = store.LoadLexicon(st)
		if err != nil {
			log.Printf("warning: lexicon load failed (%v); roots/affixes lookups will return empty", err)
			lex = &lexicon.Lexicon{}
		}
	}
	return &server{lex: lex, st: st, grammarDir: grammarDir}
}

func main() {
	dataFile := flag.String("data", store.DefaultPath(), "path to data.db")
	grammarDir := flag.String("grammar", "./docs/reference", "directory with grammar reference markdown")
	flag.Parse()

	s := newServer(*dataFile, *grammarDir)

	mcpServer := mcp.NewServer(
		&mcp.Implementation{Name: "ithkuil", Version: "v0.1.0"},
		nil,
	)
	s.registerTools(mcpServer)
	s.registerResources(mcpServer)

	if err := mcpServer.Run(context.Background(), &mcp.StdioTransport{}); err != nil {
		if err != context.Canceled {
			log.Fatalf("mcp server: %v", err)
		}
	}
	if s.st != nil {
		s.st.Close()
	}
	_ = os.Stdout.Sync()
}
