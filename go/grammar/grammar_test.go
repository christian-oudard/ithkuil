package grammar

import "testing"

func TestSlotIVToVr(t *testing.T) {
	cases := []struct {
		in   SlotIV
		want string
	}{
		{SlotIV{STA, BSC, EXS}, "a"},
		{SlotIV{STA, CTE, EXS}, "ä"},
		{SlotIV{STA, CSV, EXS}, "e"},
		{SlotIV{STA, OBJ, EXS}, "i"},
		{SlotIV{STA, BSC, FNC}, "ai"},
		{SlotIV{STA, BSC, RPS}, "ia"},
		{SlotIV{DYN, BSC, EXS}, "u"},
		{SlotIV{DYN, CTE, EXS}, "ü"},
		{SlotIV{DYN, CSV, EXS}, "o"},
		{SlotIV{DYN, OBJ, EXS}, "ö"},
		{SlotIV{DYN, BSC, AMG}, "oa"},
	}
	for _, c := range cases {
		if got := SlotIVToVr(c.in); got != c.want {
			t.Errorf("SlotIVToVr(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestSlotIIToVv(t *testing.T) {
	cases := []struct {
		in   SlotII
		want string
	}{
		{SlotII{S1, PRC}, "a"},
		{SlotII{S1, CPT}, "ä"},
		{SlotII{S2, PRC}, "e"},
		{SlotII{S2, CPT}, "i"},
		{SlotII{S3, PRC}, "u"},
		{SlotII{S3, CPT}, "ü"},
		{SlotII{S0, PRC}, "o"},
		{SlotII{S0, CPT}, "ö"},
	}
	for _, c := range cases {
		if got := SlotIIToVv(c.in); got != c.want {
			t.Errorf("SlotIIToVv(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

// TestSlotIIToVv_Panic confirms the unreachable guard fires if a caller
// builds an out-of-range SlotII. There is no valid path that reaches
// this line; the test exists purely to keep the panic from rotting.
func TestSlotIIToVv_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SlotIIToVv with bogus enum should have panicked")
		}
	}()
	SlotIIToVv(SlotII{Stem: 99, Version: 99})
}

// TestSlotIVToVr_Panic is the same guard for SlotIV.
func TestSlotIVToVr_Panic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("SlotIVToVr with bogus enum should have panicked")
		}
	}()
	SlotIVToVr(SlotIV{Function: 99, Specification: 99, Context: 99})
}

// fakeSlotVIII implements the unexported SlotVIII marker so we can drive
// DisambiguateSlotVIII into its fallthrough return — unreachable from
// the public API but worth keeping covered against future refactors.
type fakeSlotVIII struct{}

func (fakeSlotVIII) slotVIII() {}

func TestDisambiguateSlotVIII_UnknownVariantPassthrough(t *testing.T) {
	var in SlotVIII = fakeSlotVIII{}
	if got := DisambiguateSlotVIII(Ultimate, in); got != in {
		t.Errorf("unknown SlotVIII variant should pass through unchanged; got %#v", got)
	}
}

// TestDisambiguateSlotVIII_AllVariants exercises every SlotVIII variant
// (Valence, Phase, Effect, Level, Aspect) through DisambiguateSlotVIII so
// that the stress-driven Mood↔CaseScope flip is verified per variant.
// Pattern-1 forms parse as MoodVal; Ultimate stress should leave them
// unchanged; Penultimate stress should flip a freshly-constructed MoodVal
// to CaseScopeVal (and vice versa).
func TestDisambiguateSlotVIII_AllVariants(t *testing.T) {
	mood := MoodVal{Mood: SUB}
	scope := CaseScopeVal{CaseScope: CCA} // SUB ↔ CCA

	cases := []struct {
		name   string
		in     SlotVIII
		stress Stress
		want   SlotVIII
	}{
		{"Valence/verbal-noop", VnCnValence{Valence: MNO, MS: mood}, Ultimate,
			VnCnValence{Valence: MNO, MS: mood}},
		{"Valence/nominal-flip", VnCnValence{Valence: MNO, MS: mood}, Penultimate,
			VnCnValence{Valence: MNO, MS: scope}},
		{"Phase/verbal-flip", VnCnPhase{Phase: PCT, MS: scope}, Ultimate,
			VnCnPhase{Phase: PCT, MS: mood}},
		{"Phase/nominal-noop", VnCnPhase{Phase: PCT, MS: scope}, Penultimate,
			VnCnPhase{Phase: PCT, MS: scope}},
		{"Effect/verbal-flip", VnCnEffect{Effect: BEN1, MS: scope}, Antepenultimate,
			VnCnEffect{Effect: BEN1, MS: mood}},
		{"Effect/nominal-noop", VnCnEffect{Effect: BEN1, MS: scope}, Monosyllabic,
			VnCnEffect{Effect: BEN1, MS: scope}},
		{"Level/verbal-flip", VnCnLevel{Level: MIN, MS: scope}, Ultimate,
			VnCnLevel{Level: MIN, MS: mood}},
		{"Level/nominal-flip", VnCnLevel{Level: MIN, Absolute: true, MS: mood}, Penultimate,
			VnCnLevel{Level: MIN, Absolute: true, MS: scope}},
		{"Aspect/verbal-flip", VnCnAspect{Aspect: RTR, MS: scope}, Ultimate,
			VnCnAspect{Aspect: RTR, MS: mood}},
		{"Aspect/nominal-noop", VnCnAspect{Aspect: RTR, MS: scope}, Penultimate,
			VnCnAspect{Aspect: RTR, MS: scope}},
	}
	for _, c := range cases {
		if got := DisambiguateSlotVIII(c.stress, c.in); got != c.want {
			t.Errorf("%s: DisambiguateSlotVIII(%v, %#v) = %#v, want %#v",
				c.name, c.stress, c.in, got, c.want)
		}
	}
}
