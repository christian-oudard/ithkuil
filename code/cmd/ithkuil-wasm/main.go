// Command ithkuil-wasm binds the api package to a browser as
// globalThis.ithkuil.
//
// It is deliberately thin. Everything about the contract — which calls
// exist, what each answers, the exact JSON — lives in api, which builds
// on every platform and is covered by the normal suite. This file only
// converts js.Value to Go arguments and hands the reply back as a
// string. Nothing here decides anything, so nothing here can drift from
// what the tests check.
//
// Build it with tools/build_wasm.sh, which uses TinyGo. The constraint
// below is what keeps "go build ./..." and "go vet ./..." working on a
// normal host, where syscall/js does not exist.
//go:build js && wasm

package main

import (
	"syscall/js"

	"github.com/christian-oudard/ithkuil/api"
)

func main() {
	a := api.New()
	js.Global().Set("ithkuil", js.ValueOf(map[string]any{
		"info": fn(func(...string) (any, error) {
			return a.Info(), nil
		}),
		"load": fn(func(s ...string) (any, error) {
			return a.Load([]byte(arg(s, 0)))
		}),
		"parse": fn(func(s ...string) (any, error) {
			return a.Parse(arg(s, 0)), nil
		}),
		"compose": fn(func(s ...string) (any, error) {
			return a.Compose(arg(s, 0))
		}),
		"compare": fn(func(s ...string) (any, error) {
			return a.Compare(arg(s, 0), arg(s, 1))
		}),
		"search": fn(func(s ...string) (any, error) {
			return a.Search(arg(s, 0)), nil
		}),
		"define": fn(func(s ...string) (any, error) {
			return a.Define(arg(s, 0))
		}),
		"categories": fn(func(...string) (any, error) {
			return a.Categories(), nil
		}),
		"table": fn(func(s ...string) (any, error) {
			return a.Table(arg(s, 0)), nil
		}),
		"examples": fn(func(...string) (any, error) {
			return a.Examples(), nil
		}),
		"inventory": fn(func(...string) (any, error) {
			return a.Inventory(), nil
		}),
		"input": fn(func(s ...string) (any, error) {
			return a.Input(arg(s, 0)), nil
		}),
		"positions": fn(func(...string) (any, error) {
			return a.Positions(), nil
		}),
		"topics": fn(func(...string) (any, error) {
			return a.Topics(), nil
		}),
		"note": fn(func(s ...string) (any, error) {
			return a.Note(arg(s, 0))
		}),
		"affix": fn(func(s ...string) (any, error) {
			return a.Affix(arg(s, 0))
		}),
		"fromASCII": fn(func(s ...string) (any, error) {
			return a.FromASCII(arg(s, 0)), nil
		}),
	}))
	select {}
}

// fn wraps a handler as a JS callback answering in api's envelope.
func fn(h func(...string) (any, error)) js.Func {
	return js.FuncOf(func(_ js.Value, args []js.Value) any {
		strs := make([]string, len(args))
		for i, v := range args {
			strs[i] = v.String()
		}
		return api.Reply(h(strs...))
	})
}

// arg reads the i'th argument, treating a missing one as empty. A page
// calling with too few arguments gets the same answer as one passing
// empty strings, which every call already has a defined reply for.
func arg(s []string, i int) string {
	if i >= len(s) {
		return ""
	}
	return s[i]
}
