package parse

import "github.com/coudard/ithkuil/go/grammar"

// ParseVk decodes the Vk vowel (Slot IX when stress is ultimate) into
// an (Illocution, Validation) pair. Series 1 = ASR + each of the 9
// validations; Series 2 = the 8 non-ASR illocutions, each with default
// OBS validation. Stress marks are normalized away before lookup.
func ParseVk(v string) (grammar.Illocution, grammar.Validation, bool) {
	switch NormalizeAccents(v) {
	case "a":
		return grammar.ASR, grammar.OBS, true
	case "ä":
		return grammar.ASR, grammar.REC, true
	case "e":
		return grammar.ASR, grammar.PUP, true
	case "i":
		return grammar.ASR, grammar.RPR, true
	case "ëi":
		return grammar.ASR, grammar.USP, true
	case "ö":
		return grammar.ASR, grammar.IMA, true
	case "o":
		return grammar.ASR, grammar.CVN, true
	case "ü":
		return grammar.ASR, grammar.ITU, true
	case "u":
		return grammar.ASR, grammar.INF, true
	case "ai":
		return grammar.DIR, grammar.OBS, true
	case "au":
		return grammar.DEC, grammar.OBS, true
	case "ei":
		return grammar.IRG, grammar.OBS, true
	case "eu":
		return grammar.VER, grammar.OBS, true
	case "ou":
		return grammar.ADM, grammar.OBS, true
	case "oi":
		return grammar.POT, grammar.OBS, true
	case "iu":
		return grammar.HOR, grammar.OBS, true
	case "ui":
		return grammar.CNJ, grammar.OBS, true
	}
	return 0, 0, false
}
