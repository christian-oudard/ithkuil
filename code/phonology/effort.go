package phonology

// Articulatory effort, as a cost per transition between two adjacent
// segments. See docs/romanization_design.md for what this is for: one
// grammar admits several legal spellings, and the cheapest to say is
// the one to write.
//
// This ranks; it does not judge. Legality is decided by Legal and
// ClusterLegal before anything here runs, because the §2 rules are not
// pairwise (§2.13 is a trigram, §2.19 is positional, §4 judges whole
// clusters). A wrong number here yields a clumsy word, never an
// ungrammatical one.
//
// The parameters are borrowed rather than invented:
//
//   - Place and Manner coordinates and the feature saliences are
//     Kondrak's ALINE (A New Algorithm for the Alignment of Phonetic
//     Sequences, NAACL 2000), Tables 3 and 4. ALINE decomposes a
//     phoneme into exactly the features Consonant and Vowel carry.
//   - The travel term, cost rising with how far the articulators move,
//     is Kirchner (An Effort-Based Approach to Consonant Lenition,
//     1998).
//   - Keeping articulatory ease and perceptual confusability as two
//     terms rather than one number is Boersma (Functional Phonology,
//     1998).
//
// One thing had to be added rather than borrowed. ALINE measures
// similarity, for aligning cognates, so its distance falls to zero for
// identical segments. Effort does not: adjacent near-identical
// segments are penalized, not rewarded, which is the Obligatory
// Contour Principle and which is what nearly every §2 rule turns out
// to be. §2.4 and §2.5 bar homologous stops, fricatives and affricates
// disagreeing in voicing; §2.10 bars ç beside a sibilant; §2.13 bars a
// nasal plus homologous stop plus sibilant for being "too phonetically
// indistinguishable" from the same string without the stop.
//
// So the cost is U-shaped in the ALINE distance, and has two terms of
// opposite sign: travel, rising with distance, and similarity, falling
// with it. Geminates sit at distance zero and are the check on the
// second term: §1.7 permits them and bars only the triple, so the
// similarity penalty has to stay finite there.

// ALINE Table 3, multivalued feature coordinates on [0,1]. Ithkuil's
// places are §1.1's columns, which are finer than ALINE's, so each is
// mapped to the ALINE value for the nearest place it names. The two
// interpolations are noted.
var placeCoord = map[Place]float64{
	Labial:            1.00, // [bilabial]
	LabioDental:       0.95, // [labiodental]
	ApicoDental:       0.90, // [dental]
	InterDental:       0.90, // [dental]; ALINE does not separate the two
	ApicoAlveolar:     0.85, // [alveolar]
	AlveolarRetroflex: 0.80, // [retroflex]
	AlveoloPalatal:    0.75, // [palato-alveolar]
	Palatal:           0.70, // [palatal]
	Velar:             0.60, // [velar]
	Uvular:            0.50, // [uvular]
	Glottal:           0.10, // [glottal]
}

// ALINE Table 3's manner scale, which is a sonority scale read
// backwards. ALINE handles nasals and laterals as binary features
// rather than points on this scale, so they are placed here at the
// sonority Parker gives them relative to the manners ALINE does name:
// nasals above voiced fricatives, laterals above nasals, and the tap
// above the laterals.
var mannerCoord = map[Manner]float64{
	Stop:          1.00, // [stop]
	Affricate:     0.90, // [affricate]
	Fricative:     0.80, // [fricative]
	LateralFric:   0.80, // a fricative; its laterality is a separate feature
	Nasal:         0.70, // Parker: nasals are less sonorous than laterals
	LateralApprox: 0.65,
	Trill:         0.62,
	Tap:           0.61,
	Approximant:   0.60, // [approximant]
}

// ALINE Table 4, feature saliences. Manner outranks place; voicing is
// a quarter of place.
const (
	salienceManner    = 50.0
	saliencePlace     = 40.0
	salienceVoice     = 10.0
	salienceNasal     = 10.0
	salienceLateral   = 10.0
	salienceRetroflex = 10.0
	salienceHigh      = 5.0
	salienceBack      = 5.0
	salienceRound     = 5.0
	salienceSyllabic  = 5.0
)

// Distance is ALINE's δ: the weighted sum of per-feature differences
// between two phonemes. It measures how unlike they are, which is what
// ALINE was built for. It is not itself a cost; see Transition.
func Distance(a, b Phoneme) float64 {
	ca, aIsCons := a.(Consonant)
	cb, bIsCons := b.(Consonant)
	switch {
	case aIsCons && bIsCons:
		d := abs(placeCoord[ca.Place]-placeCoord[cb.Place]) * saliencePlace
		d += abs(mannerCoord[ca.Manner]-mannerCoord[cb.Manner]) * salienceManner
		d += boolDiff(ca.Voicing == Voiced, cb.Voicing == Voiced) * salienceVoice
		d += boolDiff(ca.Manner == Nasal, cb.Manner == Nasal) * salienceNasal
		d += boolDiff(isLateral(ca), isLateral(cb)) * salienceLateral
		d += boolDiff(ca.Place == AlveolarRetroflex, cb.Place == AlveolarRetroflex) *
			salienceRetroflex
		d += boolDiff(ca.Secondary == Labialized, cb.Secondary == Labialized) *
			salienceRound
		return d
	case !aIsCons && !bIsCons:
		va, vb := a.(Vowel), b.(Vowel)
		d := abs(heightCoord(va)-heightCoord(vb)) * salienceHigh
		d += abs(backCoord(va)-backCoord(vb)) * salienceBack
		d += boolDiff(va.Rounding == Rounded, vb.Rounding == Rounded) * salienceRound
		return d
	default:
		// A consonant beside a vowel. ALINE's distance is largest here,
		// because a consonant and a vowel are maximally unlike, but
		// effort is smallest: a consonant followed by a vowel is the
		// unmarked syllable, and sonority rises into a nucleus and
		// falls out of it by default. Borrowing the similarity number
		// as a cost would make every added syllable expensive, which
		// is a length penalty wearing a phonetic disguise.
		//
		// So this returns the distance at which Transition is cheapest
		// rather than a measured one. A finer model would grade it by
		// how far the consonant is from the vowel in sonority; nothing
		// yet needs that.
		return unmarkedDistance
	}
}

// Transition is the effort of saying b immediately after a. across
// says whether the pair spans a word boundary, which changes the
// answer: hh is a permissible geminate inside a word, ClusterLegal
// accepts it, and §1.6 forbids it across a boundary.
//
// The shape is U-shaped in Distance, per the note at the head of this
// file: travel rises with distance, similarity falls with it.
func Transition(a, b Phoneme, across bool) float64 {
	d := Distance(a, b)
	cost := d * travelWeight
	cost += similarityPenalty / (d + similarityFloor)
	if across && isConsonant(a) && isConsonant(b) {
		// §1.5 conditions its remedy on both sides being consonants:
		// "When a word ending in a consonant-form ... is followed in
		// the same breath-group by another word beginning with a
		// consonant-form". A vowel-final word before a consonant is
		// the case the rule is trying to reach, so it pays nothing.
		cost += boundaryPenalty
	}
	return cost
}

// The three free parameters, and the reasoning that fixes them. They
// are set rather than fitted, per docs/romanization_design.md: the
// corpus exhibits too few contrastive choices to fit several weights
// without overfitting them.
const (
	// travelWeight scales ALINE's distance, whose consonant range runs
	// to roughly 145, onto a per-transition cost of order 1.
	travelWeight = 1.0 / 145.0

	// similarityPenalty and similarityFloor give the OCP arm. The
	// floor keeps a geminate finite, since §1.7 permits geminates and
	// bars only the triple, and sets how fast the penalty decays: at
	// distance 0 the penalty is similarityPenalty/similarityFloor, and
	// it has halved by the time two segments differ by one place step
	// under salience 40.
	similarityPenalty = 8.0
	similarityFloor   = 8.0

	// unmarkedDistance is where Transition bottoms out, which is where
	// a consonant-vowel pair is placed. Derived from the other two:
	// the minimum of d*travelWeight + similarityPenalty/(d+floor) is at
	// sqrt(similarityPenalty/travelWeight) - similarityFloor.
	unmarkedDistance = 26.0

	// boundaryPenalty is the cost of a word boundary falling between
	// two consonants, which is §1.5's subject: "it is usually
	// necessary to append a vowel ... so as to avoid confusion as to
	// which word the word-final and/or word-initial consonants belong
	// to".
	//
	// It has to exceed what an added syllable costs, or the rule can
	// never fire: filling a slot replaces one C|C boundary transition
	// with a C-V transition, a V|C boundary and one more segment,
	// which is about 2*unmarkedDistance worth. "Usually necessary" is
	// the source calling that trade worth making, so this is set above
	// it rather than fitted to a count.
	boundaryPenalty = 1.4
)

func isVowelPhoneme(p Phoneme) bool {
	_, ok := p.(Vowel)
	return ok
}

func isConsonant(p Phoneme) bool {
	_, ok := p.(Consonant)
	return ok
}

func isLateral(c Consonant) bool {
	return c.Manner == LateralApprox || c.Manner == LateralFric
}

func heightCoord(v Vowel) float64 {
	// ALINE Table 3: high 1.0, mid 0.5, low 0.0.
	return [...]float64{1.0, 0.5, 0.0}[v.Height]
}

func backCoord(v Vowel) float64 {
	// ALINE Table 3: front 1.0, central 0.5, back 0.0.
	return [...]float64{1.0, 0.5, 0.0}[v.Backness]
}

func boolDiff(a, b bool) float64 {
	if a == b {
		return 0
	}
	return 1
}

func abs(f float64) float64 {
	if f < 0 {
		return -f
	}
	return f
}

// phonemeOf maps a written segment to its phoneme. Words are
// normalized and stress-stripped before they get here, so the only
// runes it should see are the 40 in the inventory.
var phonemeOf = func() map[rune]Phoneme {
	m := map[rune]Phoneme{}
	for _, e := range append(append([]PhonemeEntry{}, Consonants...), Vowels...) {
		for _, r := range e.Text {
			m[r] = e.Phoneme
			break
		}
	}
	return m
}()

// Energy is the effort of saying one word: the sum of the transition
// costs between each pair of adjacent segments. A word of one segment
// costs nothing, since there is nothing to move between.
//
// The word must already be normalized and stripped of its stress
// diacritic. An unknown rune is skipped rather than guessed at, which
// keeps a glottal stop or a stray mark from inventing a transition.
func Energy(word string) float64 {
	return spanEnergy([]string{word})
}

// TextEnergy is the effort of saying a whole span: every word's own
// transitions, plus one transition across each boundary between them.
// The boundary transitions are why this is not the sum of Energy over
// the words, and are where §1.5, §1.6, §7.1 and §7.2 live.
func TextEnergy(words []string) float64 {
	return spanEnergy(words)
}

func spanEnergy(words []string) float64 {
	var total float64
	var prev Phoneme
	var havePrev, prevWordEnd bool
	glottalStop := phonemeOf['\'']
	for _, w := range words {
		first := true
		for i, r := range w {
			p, ok := phonemeOf[r]
			if !ok {
				continue
			}
			// §1.2: "For words beginning with a glottal-stop followed
			// by a vowel, the glottal-stop is not written, however it
			// must still be pronounced." Every word begins with a
			// consonant; a vowel-initial spelling is one whose onset
			// is unwritten, and leaving it out would make the boundary
			// before it look like a free one.
			if i == 0 && isVowelPhoneme(p) {
				if havePrev {
					total += Transition(prev, glottalStop, prevWordEnd)
				}
				prev, havePrev, first = glottalStop, true, false
			}
			if havePrev {
				total += Transition(prev, p, prevWordEnd && first)
			}
			prev, havePrev, first = p, true, false
		}
		prevWordEnd = true
	}
	return total
}
