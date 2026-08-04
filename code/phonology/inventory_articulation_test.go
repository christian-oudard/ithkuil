package phonology

import "testing"

// The articulatory features exist to be measured over, not only to
// record. docs/romanization_design.md builds a phoneme-transition cost
// on top of them, and that cost reads Place as a distance: the further
// apart two values are, the further the articulators travel. That only
// holds if the enum is declared front to back, which is a property of
// the source order in inventory.go and nothing else. Written down here
// so that inserting a value in the wrong place fails rather than
// quietly skewing every pair that crosses it.
func TestPlaceIsOrderedFrontToBack(t *testing.T) {
	frontToBack := []Place{
		Labial, LabioDental, ApicoDental, InterDental, ApicoAlveolar,
		AlveolarRetroflex, AlveoloPalatal, Palatal, Velar, Uvular, Glottal,
	}
	for i := 1; i < len(frontToBack); i++ {
		if frontToBack[i-1] >= frontToBack[i] {
			t.Errorf("Place %d is not in front-to-back order with %d",
				frontToBack[i-1], frontToBack[i])
		}
	}
	if int(Glottal) != len(frontToBack)-1 {
		t.Errorf("Glottal is %d but there are %d places; a value was added "+
			"without placing it in the anatomical order",
			Glottal, len(frontToBack))
	}
}

// §1.1 gives w a Labio-velar column of its own, and §1.2.2 has u
// pronounced [ʊ] "when preceded or followed by -w- or -y-", which is a
// statement about tongue position. Recording w as plain Labial would
// make it equidistant from u and i.
func TestLabialisationIsSeparateFromPlace(t *testing.T) {
	w, ok := consonantFor("w")
	if !ok {
		t.Fatal("no entry for w")
	}
	if w.Secondary != Labialized {
		t.Errorf("w.Secondary = %v, want Labialized", w.Secondary)
	}
	if w.Place != Velar {
		t.Errorf("w.Place = %v, want Velar (the tongue gesture, per §1.1's "+
			"Labio-velar column)", w.Place)
	}
	// Every other consonant is a single constriction.
	for _, e := range Consonants {
		c, isCons := e.Phoneme.(Consonant)
		if !isCons || e.Text == "w" {
			continue
		}
		if c.Secondary != Plain {
			t.Errorf("%s has Secondary %v, want Plain", e.Text, c.Secondary)
		}
	}
}

// §1.2.2: "The uvular -ř- is an approximant [ʁ] as in colloquial French
// or German; when geminated it is either [ʁː] or can be strengthened to
// a uvular trill [ʀ]." The trill is the geminate allophone. §1.1 lists
// ř on the Approximant row, and leaves the Flap/Trill row to r alone.
func TestRhoticMannersFollowTheSource(t *testing.T) {
	for _, c := range []struct {
		text string
		want Manner
	}{
		{"ř", Approximant},
		{"r", Tap},
	} {
		got, ok := consonantFor(c.text)
		if !ok {
			t.Fatalf("no entry for %s", c.text)
		}
		if got.Manner != c.want {
			t.Errorf("%s.Manner = %v, want %v", c.text, got.Manner, c.want)
		}
	}
}

func consonantFor(text string) (Consonant, bool) {
	for _, e := range Consonants {
		if e.Text != text {
			continue
		}
		c, ok := e.Phoneme.(Consonant)
		return c, ok
	}
	return Consonant{}, false
}

// §1.1's columns, read off the PDF at the coordinates the glyphs
// actually sit at. docs/reference/morphology.md had transcribed
// several rows a column to the left, which put š ž under Alveolar
// Retroflex, ç under Alveo-palatal, h under Uvular and r under
// Apico-alveolar. The distinctions matter here because Place is
// measured over: filing t and s at one place erases a contrast §2.4
// and §2.5 turn on.
func TestPlacesFollowTheSourceColumns(t *testing.T) {
	for _, c := range []struct {
		text string
		want Place
	}{
		{"p", Labial}, {"m", Labial},
		{"f", LabioDental},
		{"t", ApicoDental}, {"d", ApicoDental}, {"n", ApicoDental},
		{"ţ", InterDental}, {"ḑ", InterDental},
		{"s", ApicoAlveolar}, {"z", ApicoAlveolar}, {"c", ApicoAlveolar},
		{"r", AlveolarRetroflex},
		{"š", AlveoloPalatal}, {"ž", AlveoloPalatal}, {"č", AlveoloPalatal},
		{"ç", Palatal}, {"y", Palatal},
		{"k", Velar}, {"ň", Velar},
		{"ř", Uvular},
		{"'", Glottal}, {"h", Glottal},
	} {
		got, ok := consonantFor(c.text)
		if !ok {
			t.Fatalf("no entry for %s", c.text)
		}
		if got.Place != c.want {
			t.Errorf("%s.Place = %v, want %v", c.text, got.Place, c.want)
		}
	}
}
