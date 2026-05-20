package parse

import "github.com/coudard/ithkuil/go/grammar"

// ParseVk decodes the Vk vowel (Slot IX when stress is ultimate) into
// a SlotIX variant. Series-1 vowels yield an Assertive with the
// corresponding Validation; the eight non-ASR diphthongs each yield
// the leaf variant for that illocution. Stress marks are normalized
// away before lookup.
func ParseVk(v string) (grammar.SlotIX, bool) {
	switch NormalizeAccents(v) {
	case "a":
		return grammar.Assertive{Validation: grammar.OBS}, true
	case "ä":
		return grammar.Assertive{Validation: grammar.REC}, true
	case "e":
		return grammar.Assertive{Validation: grammar.PUP}, true
	case "i":
		return grammar.Assertive{Validation: grammar.RPR}, true
	case "ëi":
		return grammar.Assertive{Validation: grammar.USP}, true
	case "ö":
		return grammar.Assertive{Validation: grammar.IMA}, true
	case "o":
		return grammar.Assertive{Validation: grammar.CVN}, true
	case "ü":
		return grammar.Assertive{Validation: grammar.ITU}, true
	case "u":
		return grammar.Assertive{Validation: grammar.INF}, true
	case "ai":
		return grammar.Directive{}, true
	case "au":
		return grammar.Declarative{}, true
	case "ei":
		return grammar.Interrogative{}, true
	case "eu":
		return grammar.Verificative{}, true
	case "ou":
		return grammar.Admonitive{}, true
	case "oi":
		return grammar.Potentiative{}, true
	case "iu":
		return grammar.Hortative{}, true
	case "ui":
		return grammar.Conjectural{}, true
	}
	return nil, false
}
