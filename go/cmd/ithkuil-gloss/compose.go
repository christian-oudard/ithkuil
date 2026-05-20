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

	// Stems.
	switch flag {
	case "S0":
		f.SlotII.Stem = g.S0
		return nil
	case "S1":
		f.SlotII.Stem = g.S1
		return nil
	case "S2":
		f.SlotII.Stem = g.S2
		return nil
	case "S3":
		f.SlotII.Stem = g.S3
		return nil
	}

	// Version.
	switch flag {
	case "PRC":
		f.SlotII.Version = g.PRC
		return nil
	case "CPT":
		f.SlotII.Version = g.CPT
		return nil
	}

	// Function / Specification / Context.
	switch flag {
	case "STA":
		f.SlotIV.Function = g.STA
		return nil
	case "DYN":
		f.SlotIV.Function = g.DYN
		return nil
	}
	switch flag {
	case "BSC":
		f.SlotIV.Specification = g.BSC
		return nil
	case "CTE":
		f.SlotIV.Specification = g.CTE
		return nil
	case "CSV":
		f.SlotIV.Specification = g.CSV
		return nil
	case "OBJ":
		f.SlotIV.Specification = g.OBJ
		return nil
	}
	switch flag {
	case "EXS":
		f.SlotIV.Context = g.EXS
		return nil
	case "FNC":
		f.SlotIV.Context = g.FNC
		return nil
	case "RPS":
		f.SlotIV.Context = g.RPS
		return nil
	case "AMG":
		f.SlotIV.Context = g.AMG
		return nil
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
