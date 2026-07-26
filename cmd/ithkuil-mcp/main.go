// Command ithkuil-mcp is a Model Context Protocol server that exposes
// the Ithkuil V4 grammar tooling as MCP tools and resources over
// stdio.
//
// Usage: ithkuil-mcp [-data FILE] [-grammar DIR]
//
//	-data FILE     Path to data.db (default: $XDG_DATA_HOME/ithkuil/data.db).
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

func main() {
	dataFile := flag.String("data", store.DefaultPath(), "path to data.db")
	grammarDir := flag.String("grammar", "./grammar_reference", "directory with grammar reference markdown")
	flag.Parse()

	st, err := store.Open(*dataFile)
	if err != nil {
		log.Printf("warning: cannot open data store %s (%v); roots/affixes lookups will return empty", *dataFile, err)
	}

	var lex *lexicon.Lexicon
	if st != nil {
		lex, err = lexicon.LoadFromStore(st)
		if err != nil {
			log.Printf("warning: lexicon load failed (%v); roots/affixes lookups will return empty", err)
			lex = &lexicon.Lexicon{}
		}
	} else {
		lex = &lexicon.Lexicon{}
	}

	s := &server{lex: lex, st: st, grammarDir: *grammarDir}

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
	if st != nil {
		st.Close()
	}
	_ = os.Stdout.Sync()
}
