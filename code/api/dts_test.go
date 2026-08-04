package api

import (
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"testing"
)

// The contract has two halves and only one of them is compiled here.
// web/ithkuil.d.ts is what the front end is typechecked against, and
// nothing in a Go build reads it, so a renamed field would leave the
// declaration file describing a shape the module no longer answers
// with. TypeScript would keep compiling and the page would keep
// reading undefined.
//
// So the Go types are the source and this checks the declaration
// against them: same interfaces, same fields, same optionality. A json
// tag with ",omitempty" must be declared optional and one without must
// not, because that is the difference between a field the page may
// have to handle as missing and one it may rely on.
//
// Types are matched by name. A struct here with no interface there is
// a failure, since it means a shape reaches the wire undeclared.

var (
	reIface = regexp.MustCompile(`(?m)^export interface (\w+) \{$`)
	reField = regexp.MustCompile(`(?m)^  (\w+)(\??): `)
)

// declared reads the interfaces out of the declaration file as
// name -> field -> optional.
func declared(t *testing.T) map[string]map[string]bool {
	t.Helper()
	path := filepath.Join("..", "..", "web", "ithkuil.d.ts")
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("reading the declaration file: %v", err)
	}
	out := map[string]map[string]bool{}
	for _, block := range strings.Split(string(b), "\nexport interface ") {
		name := reIface.FindStringSubmatch("export interface " + block)
		if name == nil {
			continue
		}
		body, _, _ := strings.Cut(block, "\n}")
		fields := map[string]bool{}
		for _, m := range reField.FindAllStringSubmatch(body, -1) {
			fields[m[1]] = m[2] == "?"
		}
		out[name[1]] = fields
	}
	if len(out) < 15 {
		t.Fatalf("found only %d interfaces; the parse is wrong, not the file", len(out))
	}
	return out
}

// wire lists every type that reaches the browser. A new one added to
// types.go without a line here is caught by TestWireTypesAllDeclared,
// which walks the package's own reachable types instead of this list.
func wire() []any {
	return []any{
		Error{}, Segment{}, GlossaryEntry{}, Headword{}, Word{},
		Composed{}, SlotRow{}, GlossRow{}, ComparePair{}, Unpaired{},
		Comparison{}, GrammarEntry{}, Root{}, RootHit{}, Affix{},
		SearchResult{}, Sense{}, LexiconInfo{}, Info{},
		Topic{}, Position{}, GlossToken{}, Violation{}, Member{},
		Definition{}, SearchOptions{}, AffixPage{}, RootPage{},
		Example{}, Sample{}, Input{},
	}
}

func TestDeclarationMatchesGo(t *testing.T) {
	decls := declared(t)
	for _, v := range wire() {
		rt := reflect.TypeOf(v)
		name := rt.Name()
		if name == "Error" {
			name = "ApiError" // "Error" is taken in TypeScript.
		}
		t.Run(name, func(t *testing.T) {
			fields, ok := decls[name]
			if !ok {
				t.Fatalf("%s reaches the wire but web/ithkuil.d.ts does not declare it", name)
			}
			seen := map[string]bool{}
			for i := range rt.NumField() {
				f := rt.Field(i)
				tag := f.Tag.Get("json")
				if tag == "" || tag == "-" {
					t.Errorf("%s.%s has no json tag, so its wire name is the Go name by accident", name, f.Name)
					continue
				}
				key, opts, _ := strings.Cut(tag, ",")
				seen[key] = true
				optional, declared := fields[key]
				if !declared {
					t.Errorf("%s.%s is sent as %q, which is not declared", name, f.Name, key)
					continue
				}
				if want := strings.Contains(opts, "omitempty"); optional != want {
					t.Errorf("%s.%s: declared optional=%v, json tag says %v",
						name, f.Name, optional, want)
				}
			}
			for key := range fields {
				if !seen[key] {
					t.Errorf("%s declares %q, which nothing sends", name, key)
				}
			}
		})
	}
}

// TestWireTypesAllDeclared walks outward from the types the calls
// return and fails on any struct the list in wire() missed. Without it
// a new nested type could reach the browser with no declaration and no
// test noticing.
func TestWireTypesAllDeclared(t *testing.T) {
	listed := map[string]bool{}
	for _, v := range wire() {
		listed[reflect.TypeOf(v).Name()] = true
	}
	seen := map[string]bool{}
	var walk func(rt reflect.Type)
	walk = func(rt reflect.Type) {
		for rt.Kind() == reflect.Ptr || rt.Kind() == reflect.Slice {
			rt = rt.Elem()
		}
		if rt.Kind() != reflect.Struct || seen[rt.Name()] {
			return
		}
		seen[rt.Name()] = true
		if !listed[rt.Name()] {
			t.Errorf("%s reaches the wire but wire() does not list it", rt.Name())
		}
		for i := range rt.NumField() {
			walk(rt.Field(i).Type)
		}
	}
	for _, root := range []any{Word{}, Composed{}, Comparison{}, SearchResult{}, Sense{}, Info{}, Affix{}, GrammarEntry{}, Error{}, Topic{}, Position{}, Example{}, Sample{}, Input{}} {
		walk(reflect.TypeOf(root))
	}
}

// TestJSONKeysAreCamelCase pins the naming convention. Go field names
// are capitalized and a missing tag silently sends them that way, which
// is how `Differs` and `Chunk` reached the page before this package
// existed.
func TestJSONKeysAreCamelCase(t *testing.T) {
	for _, v := range wire() {
		rt := reflect.TypeOf(v)
		for i := range rt.NumField() {
			tag := rt.Field(i).Tag.Get("json")
			key, _, _ := strings.Cut(tag, ",")
			if key == "" {
				continue
			}
			if r := rune(key[0]); r >= 'A' && r <= 'Z' {
				t.Errorf("%s.%s is sent as %q; wire keys are camelCase",
					rt.Name(), rt.Field(i).Name, key)
			}
		}
	}
}
