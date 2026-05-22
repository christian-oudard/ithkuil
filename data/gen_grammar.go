//go:build ignore

// gen_grammar prints the full grammar inventory as a JSON array to stdout.
// Used once to bootstrap the grammar section of data.json.
//
//	go run data/gen_grammar.go > /tmp/grammar.json
package main

import (
	"encoding/json"
	"os"

	"github.com/christian-oudard/ithkuil/compose"
	g "github.com/christian-oudard/ithkuil/grammar"
)

type grammarEntry struct {
	Abbrev      string `json:"abbrev"`
	Name        string `json:"name,omitempty"`
	Category    string `json:"category"`
	Form        string `json:"form,omitempty"`
	Description string `json:"description,omitempty"`
	Explanation string `json:"explanation,omitempty"`
}

func main() {
	var out []grammarEntry
	for _, e := range compose.Table {
		out = append(out, grammarEntry{
			Abbrev:      e.Abbrev,
			Name:        e.Name,
			Category:    e.Category,
			Form:        e.Form,
			Description: g.Meaning(e.Abbrev),
		})
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(out)
}
