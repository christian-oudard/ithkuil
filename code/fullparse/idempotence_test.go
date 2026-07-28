package fullparse_test

import (
	"bufio"
	"os"
	"strings"
	"testing"

	"github.com/christian-oudard/ithkuil/corpus"
	"github.com/christian-oudard/ithkuil/fullparse"
	"github.com/christian-oudard/ithkuil/phonology"
	"github.com/christian-oudard/ithkuil/render"
)

// Canonicalization has to be a fixed point. render(parse(w)) picks one
// spelling out of the several the spec permits, so feeding that back in
// must return it unchanged — otherwise the "canonical" form depends on
// how many times it has been through the pipeline, and no caller can
// tell which spelling is the settled one.
//
// This is independent of the round-trip property. Round-tripping says
// parse(render(F)) == F, which stays true even if render alternates
// between two spellings of the same formative: both parse back to F.
// Only re-rendering catches the oscillation.
//
// canonicalize returns the canonical spelling of a word, or "" if the
// word does not parse. A §3.1.7 chain is checked link by link.
func canonicalize(word string) string {
	var out []string
	for _, link := range strings.Split(word, "-") {
		f, err := fullparse.Formative(link)
		if err != nil {
			return ""
		}
		out = append(out, render.Formative(f))
	}
	return strings.Join(out, "-")
}

func assertIdempotent(t *testing.T, word string) {
	t.Helper()
	once := canonicalize(word)
	if once == "" {
		return
	}
	twice := canonicalize(once)
	if twice == "" {
		t.Errorf("%q canonicalizes to %q, which no longer parses", word, once)
		return
	}
	if twice != once {
		t.Errorf("canonicalization is not a fixed point: %q -> %q -> %q",
			word, once, twice)
	}
}

// TestIdempotent_OfficialCorpus runs every word of Quijada's 384
// example sentences through.
func TestIdempotent_OfficialCorpus(t *testing.T) {
	for _, w := range corpus.Words() {
		assertIdempotent(t, phonology.Normalize(w))
	}
}

// TestIdempotent_DiscordCorpus runs the attested-usage corpus through
// when it has been extracted. That file is generated data living
// outside the repo, so its absence skips rather than fails; the
// official corpus above is embedded and always runs.
func TestIdempotent_DiscordCorpus(t *testing.T) {
	path := os.Getenv("XDG_DATA_HOME")
	if path == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			t.Skip("no home directory")
		}
		path = home + "/.local/share"
	}
	f, err := os.Open(path + "/ithkuil/discord/extracted/v4_words.txt")
	if err != nil {
		t.Skip("no extracted Discord corpus; see tools/discord_archive")
	}
	defer f.Close()

	sc := bufio.NewScanner(f)
	for sc.Scan() {
		w := phonology.Normalize(strings.TrimSpace(sc.Text()))
		if w != "" {
			assertIdempotent(t, w)
		}
	}
}
