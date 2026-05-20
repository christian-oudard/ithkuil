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

	// Stress.
	switch flag {
	case "MON":
		f.Stress = g.Monosyllabic
		return nil
	case "PEN":
		f.Stress = g.Penultimate
		return nil
	case "ULT":
		f.Stress = g.Ultimate
		return nil
	case "ANT":
		f.Stress = g.Antepenultimate
		return nil
	}

	// Case (any of 68): treat any matching abbreviation as Slot IX case.
	for _, c := range g.AllCases {
		if c.String() == flag {
			f.SlotIX = g.CaseSlot{Case: c}
			return nil
		}
	}

	// Aspect.
	for _, a := range g.AllAspects {
		if a.String() == flag {
			f.SlotVIII = g.VnCnAspect{Aspect: a, MS: g.CaseScopeVal{CaseScope: g.CCN}}
			return nil
		}
	}

	// Valence.
	for _, v := range g.AllValences {
		if v.String() == flag {
			f.SlotVIII = g.VnCnValence{Valence: v, MS: g.MoodVal{Mood: g.FAC}}
			return nil
		}
	}

	// Mood: only meaningful with verbal stress.
	for _, m := range g.AllMoods {
		if m.String() == flag {
			// Wrap whatever Slot VIII is there with the given mood.
			switch s := f.SlotVIII.(type) {
			case g.VnCnValence:
				s.MS = g.MoodVal{Mood: m}
				f.SlotVIII = s
			case g.VnCnAspect:
				s.MS = g.MoodVal{Mood: m}
				f.SlotVIII = s
			default:
				f.SlotVIII = g.VnCnValence{Valence: g.MNO, MS: g.MoodVal{Mood: m}}
			}
			return nil
		}
	}

	// Illocution (forces ultimate stress). Looks up by tag — Assertive
	// matches "ASR" with default OBS Validation; the eight non-ASR
	// illocutions each match their own tag.
	for _, v := range g.AllIllocutionVariants {
		if v.Tag() == flag {
			f.SlotIX = v
			f.Stress = g.Ultimate
			return nil
		}
	}

	return fmt.Errorf("unknown grammar flag %q", flag)
}
