package grammar

import "testing"

// One grammatical state, one Go value. MNO Valence at the FAC
// Mood/Case-Scope is what a formative has when Slot VIII is absent, so
// a Formative that holds it and one that leaves the slot out were two
// spellings of the same word: "mlalah" against "mlala", each reading
// back as itself so no round trip noticed.
func TestSlotVIIIIsDefault(t *testing.T) {
	if !SlotVIIIIsDefault(VnCnValence{Valence: MNO, MoodScope: FAC}) {
		t.Error("MNO at FAC is what an absent Slot VIII says")
	}
	for _, s := range []SlotVIII{
		nil,
		VnCnValence{Valence: PRL, MoodScope: FAC},
		VnCnValence{Valence: MNO, MoodScope: SUB},
		VnCnPhase{Phase: PCT, MoodScope: FAC},
		VnCnAspect{Aspect: AllAspects[0], MoodScope: FAC},
	} {
		if SlotVIIIIsDefault(s) {
			t.Errorf("%v carries something an absent slot does not", s)
		}
	}
}
