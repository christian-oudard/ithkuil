package parse

import "github.com/coudard/ithkuil/go/grammar"

// ShortcutCa returns the Ca SlotVI value that a Cc shortcut encodes
// jointly with the Vv series. The mapping is from the V4 grammar Sec.
// 5 shortcut table:
//
//	Series:        1                2          3          4
//	ShortcutW: UNI/CSL/M/DEL/NRM  UNI/G/DEL  UNI/N/DEL  UNI/G/DEL/RPV
//	ShortcutY: UNI/M/PRX/NRM      UNI/M/RPV  UNI/A/DEL  UNI/M/PRX/RPV
//
// Unrecognized series fall back to the default Ca.
func ShortcutCa(s ShortcutVariant, series int) grammar.SlotVI {
	switch s {
	case ShortcutW:
		switch series {
		case 1:
			return grammar.DefaultSlotVI
		case 2:
			return grammar.SlotVI{Configuration: grammar.UNI, Affiliation: grammar.CSL, Perspective: grammar.G_, Extension: grammar.DEL, Essence: grammar.NRM}
		case 3:
			return grammar.SlotVI{Configuration: grammar.UNI, Affiliation: grammar.CSL, Perspective: grammar.N_, Extension: grammar.DEL, Essence: grammar.NRM}
		case 4:
			return grammar.SlotVI{Configuration: grammar.UNI, Affiliation: grammar.CSL, Perspective: grammar.G_, Extension: grammar.DEL, Essence: grammar.RPV}
		}
	case ShortcutY:
		switch series {
		case 1:
			return grammar.SlotVI{Configuration: grammar.UNI, Affiliation: grammar.CSL, Perspective: grammar.M_, Extension: grammar.PRX, Essence: grammar.NRM}
		case 2:
			return grammar.SlotVI{Configuration: grammar.UNI, Affiliation: grammar.CSL, Perspective: grammar.M_, Extension: grammar.DEL, Essence: grammar.RPV}
		case 3:
			return grammar.SlotVI{Configuration: grammar.UNI, Affiliation: grammar.CSL, Perspective: grammar.A_, Extension: grammar.DEL, Essence: grammar.NRM}
		case 4:
			return grammar.SlotVI{Configuration: grammar.UNI, Affiliation: grammar.CSL, Perspective: grammar.M_, Extension: grammar.PRX, Essence: grammar.RPV}
		}
	}
	return grammar.DefaultSlotVI
}

// VvSeries returns the series number (1-4) of a Vv vowel via the
// phonology vowel-form table. Unrecognized vowels default to series 1.
func VvSeries(v string) int {
	// Local import-free version: use the existing case parser to
	// recognize the vowel pattern. We can't call phonology.VowelFormLookup
	// here without an import cycle through the case file's helpers, so
	// we duplicate a minimal table.
	switch NormalizeAccents(v) {
	case "a", "ä", "e", "i", "ëi", "ö", "o", "ü", "u":
		return 1
	case "ai", "au", "ei", "eu", "ëu", "ou", "oi", "iu", "ui":
		return 2
	case "ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua",
		"uä", "uë", "üä", "üë", "öë", "öä", "ië", "iä":
		return 3
	case "ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa":
		return 4
	}
	return 1
}
