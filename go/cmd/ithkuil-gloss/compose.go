package main

import (
	"fmt"
	"io"
	"strings"

	g "github.com/coudard/ithkuil/go/grammar"
	"github.com/coudard/ithkuil/go/render"
)

// cmdCompose builds a formative from a root and a chain of grammar
// abbreviations, then renders and glosses it.
//
// Usage: ithkuil-gloss --compose ROOT [FLAGS...]
//
// Recognized FLAGS:
//
//	S0..S3       Stem (default S1)
//	PRC | CPT    Version (default PRC)
//	STA | DYN    Function (default STA)
//	BSC | CTE | CSV | OBJ
//	             Specification (default BSC)
//	EXS | FNC | RPS | AMG
//	             Context (default EXS)
//	<Case>       Slot IX case (any of 68; default THM)
//	<Aspect>     SlotVIII aspect (Pattern-2 Cn → CCN)
//	ULT | PEN | ANT | MON
//	             Stress (default Penultimate)
//
// Unrecognized flags exit non-zero.
func cmdCompose(args []string, stdout, stderr io.Writer, lexDir string) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: ithkuil-gloss --compose ROOT [FLAGS...]")
		return 2
	}
	root := args[0]
	flags := args[1:]

	f := g.MinimalFormative(root)
	for _, fl := range flags {
		if err := applyComposeFlag(&f, fl); err != nil {
			fmt.Fprintf(stderr, "compose: %v\n", err)
			return 2
		}
	}

	surface := render.Formative(f)
	fmt.Fprintln(stdout, surface)
	return 0
}

// applyComposeFlag mutates f according to one grammar-abbreviation flag.
func applyComposeFlag(f *g.Formative, flag string) error {
	flag = strings.ToUpper(flag)

	// Stems (only meaningful on CrRoot).
	switch flag {
	case "S0":
		return setStem(f, g.S0)
	case "S1":
		return setStem(f, g.S1)
	case "S2":
		return setStem(f, g.S2)
	case "S3":
		return setStem(f, g.S3)
	}

	// Version.
	switch flag {
	case "PRC":
		return setVersion(f, g.PRC)
	case "CPT":
		return setVersion(f, g.CPT)
	}

	// Function.
	switch flag {
	case "STA":
		return setFunction(f, g.STA)
	case "DYN":
		return setFunction(f, g.DYN)
	}

	// Specification (only meaningful on CrRoot/RefRoot).
	switch flag {
	case "BSC":
		return setSpecification(f, g.BSC)
	case "CTE":
		return setSpecification(f, g.CTE)
	case "CSV":
		return setSpecification(f, g.CSV)
	case "OBJ":
		return setSpecification(f, g.OBJ)
	}

	// Context.
	switch flag {
	case "EXS":
		return setContext(f, g.EXS)
	case "FNC":
		return setContext(f, g.FNC)
	case "RPS":
		return setContext(f, g.RPS)
	case "AMG":
		return setContext(f, g.AMG)
	}

	// Stress / grammatical category. MON and ULT both mean a verbal
	// formative; the renderer omits the diacritic on monosyllabic bodies
	// per §3.10.
	switch flag {
	case "MON", "ULT":
		f.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.OBS}}
		return nil
	case "PEN":
		f.Final = g.UnframedNominal{Case: caseOf(f.Final)}
		return nil
	case "ANT":
		f.Final = g.FramedVerbal{Case: caseOf(f.Final)}
		return nil
	}

	// Case (any of 68): apply to the existing Final variant. If the
	// formative is currently verbal, coerce to nominal.
	for _, c := range g.AllCases {
		if c.String() == flag {
			switch f.Final.(type) {
			case g.FramedVerbal:
				f.Final = g.FramedVerbal{Case: c}
			default:
				f.Final = g.UnframedNominal{Case: c}
			}
			return nil
		}
	}

	// Aspect.
	for _, a := range g.AllAspects {
		if a.String() == flag {
			f.SlotVIII = g.VnCnAspect{Aspect: a, MoodScope: g.FAC}
			return nil
		}
	}

	// Valence.
	for _, v := range g.AllValences {
		if v.String() == flag {
			f.SlotVIII = g.VnCnValence{Valence: v, MoodScope: g.FAC}
			return nil
		}
	}

	// Mood: replaces the MoodScope field on whatever Slot VIII is there.
	for _, m := range g.AllMoods {
		if m.String() == flag {
			switch s := f.SlotVIII.(type) {
			case g.VnCnValence:
				s.MoodScope = m
				f.SlotVIII = s
			case g.VnCnAspect:
				s.MoodScope = m
				f.SlotVIII = s
			default:
				f.SlotVIII = g.VnCnValence{Valence: g.MNO, MoodScope: m}
			}
			return nil
		}
	}

	// Illocution: sets UnframedVerbal with the Vk variant. Looks up by
	// tag — "ASR" yields Assertive with default OBS Validation; the
	// eight non-ASR illocutions each match their own tag.
	for _, v := range g.AllVk {
		if v.Tag() == flag {
			f.Final = g.UnframedVerbal{Vk: v}
			return nil
		}
	}

	return fmt.Errorf("unknown grammar flag %q", flag)
}

// caseOf extracts the Case from a Final variant that carries one,
// defaulting to THM for verbal Finals.
func caseOf(f g.Final) g.Case {
	switch v := f.(type) {
	case g.UnframedNominal:
		return v.Case
	case g.FramedVerbal:
		return v.Case
	}
	return g.THM
}

// Root-field setters. Each rewrites the relevant field of whichever
// Root variant is present; setters that don't apply to a variant
// return an error so the user sees their flag was ignored.

func setStem(f *g.Formative, s g.Stem) error {
	cr, ok := f.Root.(g.CrRoot)
	if !ok {
		return fmt.Errorf("stem flag only applies to CrRoot")
	}
	cr.Stem = s
	f.Root = cr
	return nil
}

func setVersion(f *g.Formative, v g.Version) error {
	switch r := f.Root.(type) {
	case g.CrRoot:
		r.Version = v
		f.Root = r
	case g.CsRoot:
		r.Version = v
		f.Root = r
	case g.RefRoot:
		r.Version = v
		f.Root = r
	default:
		return fmt.Errorf("no root to set version on")
	}
	return nil
}

func setFunction(f *g.Formative, fn g.Function) error {
	switch r := f.Root.(type) {
	case g.CrRoot:
		r.SlotIV.Function = fn
		f.Root = r
	case g.CsRoot:
		r.Function = fn
		f.Root = r
	case g.RefRoot:
		r.SlotIV.Function = fn
		f.Root = r
	default:
		return fmt.Errorf("no root to set function on")
	}
	return nil
}

func setSpecification(f *g.Formative, sp g.Specification) error {
	switch r := f.Root.(type) {
	case g.CrRoot:
		r.SlotIV.Specification = sp
		f.Root = r
	case g.RefRoot:
		r.SlotIV.Specification = sp
		f.Root = r
	default:
		return fmt.Errorf("specification only applies to CrRoot/RefRoot")
	}
	return nil
}

func setContext(f *g.Formative, ctx g.Context) error {
	switch r := f.Root.(type) {
	case g.CrRoot:
		r.SlotIV.Context = ctx
		f.Root = r
	case g.CsRoot:
		r.Context = ctx
		f.Root = r
	case g.RefRoot:
		r.SlotIV.Context = ctx
		f.Root = r
	default:
		return fmt.Errorf("no root to set context on")
	}
	return nil
}
