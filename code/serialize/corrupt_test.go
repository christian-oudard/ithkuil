package serialize

import (
	"testing"

	g "github.com/christian-oudard/ithkuil/grammar"
)

// everyWordShape is one value per grammar.Word variant, each with its
// optional parts filled, so a sweep over it reaches every decoder.
// A shape missing here is a decoder nothing corrupts.
func everyWordShape() []g.Word {
	dat := g.DAT
	nomic := g.Nomic
	f := g.MinimalFormative("m")
	full := g.MinimalFormative("kš")
	full.SlotV = []g.Affix{{Consonant: "r", Type: g.Type1Affix, Degree: 3}}
	full.SlotVI = g.SlotVI{Configuration: g.DSS, Affiliation: g.COA, Perspective: g.G_, Extension: g.ICP, Essence: g.RPV}
	full.SlotVII = []g.Affix{{Consonant: "t", Type: g.Type2Affix, Degree: 7}}
	full.SlotVIII = g.VnCnAspect{Aspect: g.RTR, MoodScope: g.SUB}
	full.Final = g.UnframedVerbal{Vk: g.Assertive{Validation: g.REC}}

	chain := g.NewChain(full).AddType1(f)
	return []g.Word{
		f,
		full,
		chain,
		g.DOL,
		g.RegisterMarker{Register: g.DSV},
		g.RegisterMarker{Register: g.CGT, End: true},
		g.CarrierAdjunct{Type: g.Carrier, Case: g.ERG},
		g.ModularAdjunct{},
		g.ModularAdjunct{
			Scope:   g.ModularScopeParent,
			Reach:   g.ModularReachFormative,
			Content: []g.SlotVIII{g.VnCnValence{Valence: g.PRL, MoodScope: g.HYP}},
		},
		g.SingleAffixAdjunct{Affix: g.Affix{Consonant: "r", Type: g.Type1Affix, Degree: 3}},
		g.MultipleAffixAdjunct{
			First:      g.Affix{Consonant: "r", Type: g.Type1Affix, Degree: 3},
			Rest:       []g.Affix{{Consonant: "t", Type: g.Type2Affix, Degree: 5}},
			FirstScope: g.ScopeVDom,
			RestScope:  g.ScopeVIIDom,
		},
		g.Referential{
			Head: g.PersonalHead{
				Refs: []g.PersonalRef{
					{Referent: g.R1m, Effect: g.NEU},
					{Referent: g.R2p, Effect: g.BEN},
				},
				Category: &nomic,
			},
			Case: g.THM,
			Second: &g.SecondReferent{
				Case: g.DAT,
				Refs: []g.PersonalRef{{Referent: g.R2m, Effect: g.DET}},
			},
			RpvEssence: true,
		},
		g.CombinationReferential{
			Head:    g.PersonalHead{Refs: []g.PersonalRef{{Referent: g.R1m, Effect: g.NEU}}},
			Case:    g.THM,
			Spec:    g.CTE,
			Affixes: []g.Affix{{Consonant: "r", Type: g.Type1Affix, Degree: 3}},
			Case2:   &dat,
		},
		g.Foreign{Text: "Vancouver"},
	}
}

// TestUnmarshal_Truncated is the property a binary format needs and no
// round trip can show: bytes read back are bytes from a file, which may
// be short, and a decoder that indexes past the end takes the process
// with it. Every prefix of a valid encoding must come back as an error.
//
// The full-length case is excluded, being the one that must succeed.
func TestUnmarshal_Truncated(t *testing.T) {
	for _, w := range everyWordShape() {
		b, err := MarshalWord(w)
		if err != nil {
			t.Errorf("MarshalWord(%T): %v", w, err)
			continue
		}
		for n := 0; n < len(b); n++ {
			func() {
				defer func() {
					if r := recover(); r != nil {
						t.Errorf("%T truncated to %d/%d bytes panicked: %v", w, n, len(b), r)
					}
				}()
				if _, _, err := UnmarshalWord(b[:n]); err == nil {
					t.Errorf("%T truncated to %d/%d bytes decoded without error", w, n, len(b))
				}
			}()
		}
	}
}

// TestUnmarshal_Corrupted flips each byte of each encoding to a spread
// of values and requires the decoder to answer, not to die. Reading a
// wrong value back is allowed: the format is dense and most bytes are
// meaningful, so many flips are simply a different valid word. Panicking
// is not, and neither is looping.
func TestUnmarshal_Corrupted(t *testing.T) {
	// A spread rather than all 256: the low values exercise tags and
	// small enums, the high ones the continuation bit of a uvarint and
	// out-of-range enum values.
	flips := []byte{0x00, 0x01, 0x07, 0x3f, 0x40, 0x7f, 0x80, 0xfe, 0xff}
	for _, w := range everyWordShape() {
		b, err := MarshalWord(w)
		if err != nil {
			continue // TestUnmarshal_Truncated reports this.
		}
		for i := range b {
			for _, v := range flips {
				if b[i] == v {
					continue
				}
				corrupt := append([]byte(nil), b...)
				corrupt[i] = v
				func() {
					defer func() {
						if r := recover(); r != nil {
							t.Fatalf("%T with byte %d set to %#x panicked: %v", w, i, v, r)
						}
					}()
					_, _, _ = UnmarshalWord(corrupt)
				}()
			}
		}
	}
}

// TestUnmarshalTokens_Truncated does the same for a stream, where a
// short read can also fall between two words rather than inside one.
func TestUnmarshalTokens_Truncated(t *testing.T) {
	b, err := MarshalTokens(everyWordShape())
	if err != nil {
		t.Fatalf("MarshalTokens: %v", err)
	}
	for n := 0; n < len(b); n++ {
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("stream truncated to %d/%d bytes panicked: %v", n, len(b), r)
				}
			}()
			_, _ = UnmarshalTokens(b[:n])
		}()
	}
	got, err := UnmarshalTokens(b)
	if err != nil {
		t.Fatalf("the whole stream must decode: %v", err)
	}
	if len(got) != len(everyWordShape()) {
		t.Errorf("stream decoded %d words, wrote %d", len(got), len(everyWordShape()))
	}
}

// TestUnmarshal_ReportsLength pins the count UnmarshalWord returns
// beside the word. It is how UnmarshalTokens finds the next word, so a
// decoder that read the right value but reported the wrong length would
// round-trip a single word perfectly and corrupt every stream of two.
func TestUnmarshal_ReportsLength(t *testing.T) {
	for _, w := range everyWordShape() {
		b, err := MarshalWord(w)
		if err != nil {
			continue // TestUnmarshal_Truncated reports this.
		}
		// Trailing bytes must not change the reading or the count: in a
		// stream, everything after the word is trailing bytes.
		got, n, err := UnmarshalWord(append(b, 0xff, 0x00, 0x7f))
		if err != nil {
			t.Errorf("%T with a stream after it: %v", w, err)
			continue
		}
		if n != len(b) {
			t.Errorf("%T consumed %d bytes, was written in %d", w, n, len(b))
		}
		again, err := MarshalWord(got)
		if err != nil {
			t.Errorf("%T did not survive to re-marshal: %v", w, err)
			continue
		}
		if string(again) != string(b) {
			t.Errorf("%T re-marshals differently\n  wrote %x\n  got   %x", w, b, again)
		}
	}
}
