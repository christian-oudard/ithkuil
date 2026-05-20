package main

import (
	"flag"
	"fmt"
	"io"
)

// flagSet wraps a flag.FlagSet with a registry of (long, short, usage)
// triples so we can print POSIX-style help text ("--long, -s").
type flagSet struct {
	*flag.FlagSet
	pairs   []flagPair
	summary string // one-line description shown above the flag list
	args    string // e.g. "TEXT..." or "QUERY"
}

type flagPair struct {
	long  string
	short string
	usage string
	value string // empty for bools; "DIR", "N", "root|affix|both" for typed flags
}

func newFlagSet(name string, out io.Writer) *flagSet {
	fs := &flagSet{FlagSet: flag.NewFlagSet(name, flag.ContinueOnError)}
	fs.SetOutput(out)
	fs.Usage = func() { fs.printUsage(out) }
	return fs
}

func (fs *flagSet) describe(summary, args string) {
	fs.summary = summary
	fs.args = args
}

func (fs *flagSet) Bool(long, short string, def bool, usage string) *bool {
	v := fs.FlagSet.Bool(long, def, "")
	if short != "" {
		fs.FlagSet.BoolVar(v, short, def, "")
	}
	fs.pairs = append(fs.pairs, flagPair{long, short, usage, ""})
	return v
}

func (fs *flagSet) String(long, short, def, valueLabel, usage string) *string {
	v := fs.FlagSet.String(long, def, "")
	if short != "" {
		fs.FlagSet.StringVar(v, short, def, "")
	}
	fs.pairs = append(fs.pairs, flagPair{long, short, usage, valueLabel})
	return v
}

func (fs *flagSet) Int(long, short string, def int, valueLabel, usage string) *int {
	v := fs.FlagSet.Int(long, def, "")
	if short != "" {
		fs.FlagSet.IntVar(v, short, def, "")
	}
	fs.pairs = append(fs.pairs, flagPair{long, short, usage, valueLabel})
	return v
}

func (fs *flagSet) printUsage(w io.Writer) {
	fmt.Fprintf(w, "usage: ithkuil %s", fs.Name())
	if len(fs.pairs) > 0 {
		fmt.Fprint(w, " [flags]")
	}
	if fs.args != "" {
		fmt.Fprintf(w, " %s", fs.args)
	}
	fmt.Fprintln(w)
	if fs.summary != "" {
		fmt.Fprintln(w)
		fmt.Fprintln(w, "  "+fs.summary)
	}
	if len(fs.pairs) == 0 {
		return
	}
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Flags:")
	for _, p := range fs.pairs {
		switch {
		case p.short != "" && p.value != "":
			fmt.Fprintf(w, "  --%s %s, -%s %s\n        %s\n",
				p.long, p.value, p.short, p.value, p.usage)
		case p.short != "":
			fmt.Fprintf(w, "  --%s, -%s\n        %s\n", p.long, p.short, p.usage)
		case p.value != "":
			fmt.Fprintf(w, "  --%s %s\n        %s\n", p.long, p.value, p.usage)
		default:
			fmt.Fprintf(w, "  --%s\n        %s\n", p.long, p.usage)
		}
	}
}
