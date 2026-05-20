package parse

import "github.com/coudard/ithkuil/go/grammar"

// valenceVowels maps Vn vowels to Valence (Pattern 1, Series 1).
var valenceVowels = map[string]grammar.Valence{
	"a": grammar.MNO, "ä": grammar.PRL, "e": grammar.CRO,
	"i": grammar.RCP, "ëi": grammar.CPL, "ö": grammar.DUP,
	"o": grammar.DEM, "ü": grammar.CNG, "u": grammar.PTI,
}

// phaseVowels maps Vn vowels to Phase (Pattern 1, Series 2).
var phaseVowels = map[string]grammar.Phase{
	"ai": grammar.PCT, "au": grammar.ITR, "ei": grammar.REP,
	"eu": grammar.ITM, "ëu": grammar.RCT, "ou": grammar.FRE,
	"oi": grammar.FRG, "iu": grammar.VAC, "ui": grammar.FLC,
}

// effectVowels maps Vn vowels to Effect (Pattern 1, Series 3). Includes
// the y-/w-glide alternates that resolve to the same Effect.
var effectVowels = map[string]grammar.Effect{
	"ia": grammar.BEN1, "ie": grammar.BEN2, "io": grammar.BEN3,
	"iö": grammar.BSLF, "eë": grammar.UNK, "uö": grammar.DSLF,
	"uo": grammar.DET3, "ue": grammar.DET2, "ua": grammar.DET1,
	// Series 3 alternates
	"uä": grammar.BEN1, "uë": grammar.BEN2, "üä": grammar.BEN3,
	"üë": grammar.BSLF, "öë": grammar.DSLF, "öä": grammar.DET3,
	"ië": grammar.DET2, "iä": grammar.DET1,
}

// levelVowels maps Vn vowels to Level (Pattern 1, Series 4).
var levelVowels = map[string]grammar.Level{
	"ao": grammar.MIN, "aö": grammar.SBE, "eo": grammar.IFR,
	"eö": grammar.DFT, "oë": grammar.EQU, "öe": grammar.SUR,
	"oe": grammar.SPL, "öa": grammar.SPQ, "oa": grammar.MAX,
}

// aspectVowels maps Vn vowels to Aspect (Pattern 2, all 4 series + the
// series-3 alternates).
var aspectVowels = map[string]grammar.Aspect{
	// Column 1 (Series 1)
	"a": grammar.RTR, "ä": grammar.PRS, "e": grammar.HAB,
	"i": grammar.PRG, "ëi": grammar.IMM, "ö": grammar.PCS,
	"o": grammar.REG, "ü": grammar.SMM, "u": grammar.ATP,
	// Column 2 (Series 2)
	"ai": grammar.RSM, "au": grammar.CSS, "ei": grammar.PAU,
	"eu": grammar.RGR, "ëu": grammar.PCL, "ou": grammar.CNT,
	"oi": grammar.ICS, "iu": grammar.EXP, "ui": grammar.IRP,
	// Column 3 (Series 3, canonical)
	"ia": grammar.PMP, "ie": grammar.CLM, "io": grammar.DLT,
	"iö": grammar.TMP, "eë": grammar.XPD, "uö": grammar.LIM,
	"uo": grammar.EPD, "ue": grammar.PTC, "ua": grammar.PPR,
	// Column 3 alternates
	"uä": grammar.PMP, "uë": grammar.CLM, "üä": grammar.DLT,
	"üë": grammar.TMP, "öë": grammar.LIM, "öä": grammar.EPD,
	"ië": grammar.PTC, "iä": grammar.PPR,
	// Column 4 (Series 4)
	"ao": grammar.DCL, "aö": grammar.CCL, "eo": grammar.CUL,
	"eö": grammar.IMD, "oë": grammar.TRD, "öe": grammar.TNS,
	"oe": grammar.ITC, "öa": grammar.MTV, "oa": grammar.SQN,
}

// ParseVnValence decodes a Vn vowel as a Valence (Pattern 1, Series 1).
func ParseVnValence(v string) (grammar.Valence, bool) {
	val, ok := valenceVowels[NormalizeAccents(v)]
	return val, ok
}

// ParseVnPhase decodes a Vn vowel as a Phase (Pattern 1, Series 2).
func ParseVnPhase(v string) (grammar.Phase, bool) {
	p, ok := phaseVowels[NormalizeAccents(v)]
	return p, ok
}

// ParseVnEffect decodes a Vn vowel as an Effect (Pattern 1, Series 3).
func ParseVnEffect(v string) (grammar.Effect, bool) {
	e, ok := effectVowels[NormalizeAccents(v)]
	return e, ok
}

// ParseVnLevel decodes a Vn vowel as a Level (Pattern 1, Series 4).
func ParseVnLevel(v string) (grammar.Level, bool) {
	l, ok := levelVowels[NormalizeAccents(v)]
	return l, ok
}

// ParseVnAspect decodes a Vn vowel as an Aspect (Pattern 2, any series).
func ParseVnAspect(v string) (grammar.Aspect, bool) {
	a, ok := aspectVowels[NormalizeAccents(v)]
	return a, ok
}

// ParseCnMood decodes a Pattern-1 Cn consonant as a Mood.
func ParseCnMood(c string) (grammar.Mood, bool) {
	switch c {
	case "h":
		return grammar.FAC, true
	case "hl":
		return grammar.SUB, true
	case "hr":
		return grammar.ASM, true
	case "hm":
		return grammar.SPC, true
	case "hn":
		return grammar.COU, true
	case "hň":
		return grammar.HYP, true
	}
	return 0, false
}

// ParseCnMoodP2 decodes a Pattern-2 Cn consonant (used with Aspect Vn)
// as a Mood. "w" and "y" both encode FAC.
func ParseCnMoodP2(c string) (grammar.Mood, bool) {
	switch c {
	case "w", "y":
		return grammar.FAC, true
	case "hw":
		return grammar.SUB, true
	case "hrw":
		return grammar.ASM, true
	case "hmw":
		return grammar.SPC, true
	case "hnw":
		return grammar.COU, true
	case "hňw":
		return grammar.HYP, true
	}
	return 0, false
}

// ParseCnCaseScope decodes a Cn consonant (either pattern) as a
// CaseScope. Pattern-1 and Pattern-2 variants of the same value collapse
// to one CaseScope.
func ParseCnCaseScope(c string) (grammar.CaseScope, bool) {
	switch c {
	case "h", "w", "y":
		return grammar.CCN, true
	case "hl", "hw":
		return grammar.CCA, true
	case "hr", "hrw":
		return grammar.CCS, true
	case "hm", "hmw":
		return grammar.CCQ, true
	case "hn", "hnw":
		return grammar.CCP, true
	case "hň", "hňw":
		return grammar.CCV, true
	}
	return 0, false
}

// validCn enumerates the consonants accepted as Cn (Slot VIII tail).
// h/hl/hr/hm/hn/hň are Pattern 1; w/y/hw/hrw/hmw/hnw/hňw are Pattern 2.
var validCn = map[string]bool{
	"h": true, "hl": true, "hr": true, "hm": true, "hn": true, "hň": true,
	"w": true, "y": true, "hw": true, "hrw": true, "hmw": true, "hnw": true, "hňw": true,
}

// IsValidCn reports whether c is a recognized Slot VIII Cn consonant.
func IsValidCn(c string) bool { return validCn[c] }

// IsPattern2Cn reports whether c is a Pattern-2 Cn (paired with Aspect
// Vn, not Valence/Phase/Effect/Level).
func IsPattern2Cn(c string) bool {
	switch c {
	case "w", "y", "hw", "hrw", "hmw", "hnw", "hňw":
		return true
	}
	return false
}

// ParseVnCn parses a Vn vowel + Cn consonant into one of the five
// SlotVIII variants. The returned MoodScope is the initial parse:
// The Cn consonant encodes a value shared by Mood and CaseScope; this
// parser stores it as a grammar.Mood (the labelling depends on the
// formative's Final category, applied at gloss time).
//
// Returns (nil, false) if Cn is not a valid Slot VIII consonant or Vn
// doesn't match any category in the corresponding pattern.
func ParseVnCn(vn, cn string) (grammar.SlotVIII, bool) {
	var ms grammar.Mood
	if m, ok := ParseCnMood(cn); ok {
		ms = m
	} else if cs, ok := ParseCnCaseScope(cn); ok {
		ms = grammar.CaseScopeToMood(cs)
	} else {
		return nil, false
	}

	if IsPattern2Cn(cn) {
		asp, ok := ParseVnAspect(vn)
		if !ok {
			return nil, false
		}
		return grammar.VnCnAspect{Aspect: asp, MoodScope: ms}, true
	}

	// Pattern 1: probe in declaration order
	// (Valence → Phase → Effect → Level by vowel series).
	if val, ok := ParseVnValence(vn); ok {
		return grammar.VnCnValence{Valence: val, MoodScope: ms}, true
	}
	if ph, ok := ParseVnPhase(vn); ok {
		return grammar.VnCnPhase{Phase: ph, MoodScope: ms}, true
	}
	if eff, ok := ParseVnEffect(vn); ok {
		return grammar.VnCnEffect{Effect: eff, MoodScope: ms}, true
	}
	if lvl, ok := ParseVnLevel(vn); ok {
		return grammar.VnCnLevel{Level: lvl, Absolute: false, MoodScope: ms}, true
	}
	return nil, false
}
