package slots

// FormativeCorpus is a curated list of romanizations that exercise
// the formative slot grammar across its non-trivial paths: minimal /
// canonical forms, the concatenation prefixes, every shortcut variant
// (Cc, §3.8.1.2 Cn→Ca, §3.6.2 Slot V end-marker), Cs-root and
// reference-root formatives, Slot V affix stacks, and sentence-start
// prefixes.
//
// Layer C must round-trip every entry (slots.Parse / slots.Render
// agree). Other layers — fullparse, gloss, compose — consume the
// same list to assert their own round-trip invariants on the same
// well-known set.
var FormativeCorpus = []string{
	// Canonical and minimal.
	"maţřëullait",
	"malëuţřait",
	"amlal",
	"amlala",
	"amlalú",
	"emlölo",
	"malal",
	// Concat prefixes.
	"hamlala",
	"hwamlala",
	// Shortcut forms.
	"waml",
	"yuml",
	"waiml",
	"wamlar",
	"hlaml",
	// Cs-root and reference-root.
	"ëilal",
	"ëilael",
	"oërmölá",
	"oërmoulá",
	"ealali",
	"aelali",
	// Slot V (multiple affixes between Vv and Ca).
	"amlalahla",
	"amlalahlá",
	"amlalara",
	"amlali'a",
	"ärmaläwi'a",
	// §3.8.1.2 Cn→Ca shortcut: a Pattern-1 Cn (hl/hr/hm/hn/hň) in the
	// Ca slot, eliding default -l- Ca and default -a- Vn.
	"amlahla",
	"amlahra",
	"amlahma",
	"amlahna",
	"amlahňa",
	// §3.6.2 shortcut-form Slot V: a single affix between Cr and Vc,
	// with a glottal-stop end-of-Slot-V marker on the final Vx.
	"wamla'r",
	"wamla're",
}
