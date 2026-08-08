package phonology

import "math"

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
		// Unreachable from Transition, which handles the mixed case
		// before asking for a distance. Kept total so that Distance is
		// still a function of any two phonemes.
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
	// A consonant beside a vowel does not go through the U at all. The
	// U models one constriction giving way to another, and a syllable
	// is not that. An onset opening into a nucleus is the easiest
	// thing the vocal tract does, and it is what every word is made
	// of. Sending it through the curve priced it as the least bad
	// consonant cluster, which is not the same as easy, and that made
	// every added syllable dear enough to need an inflated boundary
	// penalty to overcome.
	if isConsonant(a) != isConsonant(b) {
		// A glottal stop is not an unmarked onset. It is a complete
		// closure of the glottis, so it interrupts the voice stream
		// rather than opening into it, and §1.7 spends a whole section
		// on where one may be put. Pricing it as an ordinary syllable
		// made a'u cheaper than the diphthong au.
		if isGlottalStop(a) || isGlottalStop(b) {
			return glottalStopCost
		}
		return syllableCost
	}
	// A permissible diphthong is one gesture, not two vowels colliding:
	// §1.2.1 names the ten falling diphthongs, and the tongue glides
	// through them without a second constriction. Sending them through
	// the curve charged them the similarity penalty meant for two
	// consonants at the same place.
	if va, ok := a.(Vowel); ok {
		if vb, ok := b.(Vowel); ok && isPermissibleDiphthong(va, vb) {
			return syllableCost
		}
	}
	// §1.7 permits geminates and bars only the triple, and the speaker
	// splits them: alla is easier than alra, while assa and atta are
	// harder than asta. A repeated sonorant is one long gesture; a
	// repeated obstruent means holding a constriction, which is work.
	if ca, ok := a.(Consonant); ok && a == b && isObstruent(ca) {
		return obstruentGeminateCost
	}
	d := Distance(a, b)
	cost := d*travelWeight + risingSonority(a, b)
	cost += ocp(d)
	if across && isConsonant(a) && isConsonant(b) && !isGlottalStop(b) {
		// §1.5 conditions its remedy on both sides being consonants:
		// "When a word ending in a consonant-form ... is followed in
		// the same breath-group by another word beginning with a
		// consonant-form". A vowel-final word before a consonant is
		// the case the rule is trying to reach, so it pays nothing.
		//
		// Nor does a following glottal stop. §1.5's harm is "confusion
		// as to which word the word-final and/or word-initial
		// consonants belong to", and a glottal onset is the thing that
		// says a word began here: §1.2 has it unwritten but pronounced
		// on every vowel-initial word. There is nothing to be confused
		// about, so there is nothing to remedy.
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

	// unmarkedDistance is where the curve bottoms out. Nothing is
	// placed there any more; it survives so that Distance is still
	// total over any two phonemes, and so that the tests can say where
	// the minimum is.
	unmarkedDistance = 26.0

	// syllableCost is a consonant opening into a vowel, or closing out
	// of one. Small rather than zero: it is still a gesture, but it is
	// the unmarked one, and cheaper than any consonant meeting another
	// consonant.
	syllableCost = 0.05

	// glottalStopCost is a glottal stop beside a vowel. A complete
	// closure interrupting the voice stream, so dearer than an
	// ordinary onset, and dear enough that a diphthong beats the same
	// two vowels split by one. §1.7 treats its placement as something
	// to be decided rather than done freely.
	glottalStopCost = 0.5

	// segmentCost is charged once per segment, and is the only reason
	// a longer word costs more than a shorter one. Length belongs here
	// rather than in the transitions: saying more costs more because
	// there are more gestures, which is Kirchner's per-gesture effort,
	// not because any junction between them is hard.
	segmentCost = 0.05

	// boundaryPenalty is what a word boundary adds to a consonant
	// meeting a consonant, which is §1.5's subject: "it is usually
	// necessary to append a vowel ... so as to avoid confusion as to
	// which word the word-final and/or word-initial consonants belong
	// to". §1.6 and §7 are measured in the same units.
	boundaryPenalty = 0.5
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
			total += SegmentCost(p)
			total += SegmentCost(p)
			if havePrev {
				total += Transition(prev, p, prevWordEnd && first)
			}
			prev, havePrev, first = p, true, false
		}
		prevWordEnd = true
	}
	return total
}

func isGlottalStop(p Phoneme) bool {
	c, ok := p.(Consonant)
	return ok && c.Place == Glottal && c.Manner == Stop
}

// textOf is the reverse of phonemeOf, for the few places that need to
// ask a table keyed by spelling about a phoneme.
var textOf = func() map[Phoneme]string {
	m := map[Phoneme]string{}
	for _, e := range append(append([]PhonemeEntry{}, Consonants...), Vowels...) {
		m[e.Phoneme] = e.Text
	}
	return m
}()

func isPermissibleDiphthong(a, b Vowel) bool {
	return permissibleDiphthongs[textOf[a]+textOf[b]]
}

// Everything below is fitted to twenty pairwise judgments from a
// speaker; see effort_judgment_test.go, which holds them. The
// borrowed parameters above answer "how unlike are these two
// segments", which is what ALINE was built for and is not the same
// question. These answer "which is harder to say", which is.
//
// The numbers are therefore ours and the citations do not cover them.
// What is borrowed here is only the vocabulary: place, manner, voicing
// and rounding are the axes; the weights on them are the speaker's.

// SegmentCost is the effort of producing one segment, before anything
// about what sits beside it. Most of the speaker's judgments are of
// this kind rather than about a junction: l easier than r easier than
// ř, y easier than w, i easier than u, s easier than ţ and than š,
// voiceless easier than voiced, dental easier than bilabial.
// isRhotic is r and ř. The speaker put ř last in every pair it
// appeared in, arta over ařta, axta over ařta and axla over ařla, and
// graded x merely "hard-ish". So what is expensive about ř is being a
// rhotic and not being uvular: pricing the place instead made x dear
// too, which got axla against aţla backwards.
func isRhotic(c Consonant) bool {
	return c.Place == AlveolarRetroflex || (c.Place == Uvular && c.Manner == Approximant)
}

// isObstruent is a stop, affricate or fricative: a segment made with a
// real constriction, as against the sonorants. Geminates split on this.
// The speaker found alla easy and assa and atta not, which is holding
// the constriction rather than repeating the segment, and is Kirchner's
// account of why geminates resist lenition.
func isObstruent(c Consonant) bool {
	switch c.Manner {
	case Stop, Affricate, Fricative, LateralFric:
		return true
	}
	return false
}

func SegmentCost(p Phoneme) float64 {
	switch v := p.(type) {
	case Vowel:
		c := baseSegment
		if v.Rounding == Rounded {
			// The speaker preferred i to u and y to w, giving the same
			// reason both times: rounding is extra work.
			c += roundingCost
		}
		return c
	case Consonant:
		c := baseSegment + placeCost[v.Place] + mannerCost[v.Manner]
		if v.Voicing == Voiced {
			// "voicedness is harder": holding voicing through a
			// constriction is an active gesture, which is also why
			// Kirchner's geminates resist lenition.
			c += voicingCost
		}
		if v.Secondary == Labialized {
			c += roundingCost
		}
		if isRhotic(v) {
			c += rhoticCost
			if v.Place == Uvular {
				// ř over r, consistently.
				c += rhoticCost / 2
			}
		}
		return c
	}
	return baseSegment
}

// placeCost ranks the places by how much work the speaker found them.
// Apico-alveolar is the rest position and cheapest; the labials cost
// because the lips must move, and the back places because the tongue
// body must. Fitted, not measured.
var placeCost = map[Place]float64{
	ApicoAlveolar:     0.00, // s z c ẓ l ļ, the cheapest
	ApicoDental:       0.01, // t d n; the speaker put these under the labials
	InterDental:       0.04, // ţ ḑ, dearer than s
	AlveolarRetroflex: 0.03, // r, dearer than l
	AlveoloPalatal:    0.05, // š ž č j, dearer than s
	Palatal:           0.02, // ç y
	Labial:            0.03, // p b m, dearer than the dentals
	LabioDental:       0.02, // f v
	Velar:             0.02, // k g ň w
	Uvular:            0.04, // x ř; the speaker graded x "hard-ish"
	Glottal:           0.02, // ' h
}

// mannerCost separates l from r beyond what place already does, and
// keeps the obstruents above the sonorants.
var mannerCost = map[Manner]float64{
	Stop:          0.03,
	Affricate:     0.03,
	Fricative:     0.02,
	LateralFric:   0.02,
	Nasal:         0.01,
	Tap:           0.02, // r: a ballistic gesture, dearer than a lateral
	Trill:         0.04,
	LateralApprox: 0.00, // l: the cheapest consonant the speaker named
	Approximant:   0.01,
}

// risingSonority charges a cluster whose sonority rises. The speaker
// preferred alta to atla, "less stoppage of airflow": in alta the
// liquid closes one syllable and the stop opens the next, while atla
// asks for a stop released straight into a liquid. Nothing else in the
// model can see this, because Distance is symmetric.
//
// The sources are directional in the same way. §2.5 permits cč and cj
// while barring čc and čẓ; §2.9 bars an affricate before a sibilant
// fricative but not the reverse; §2.18 bars dļ, gļ and bļ but not ļd,
// ļg or ļb.
func risingSonority(a, b Phoneme) float64 {
	ca, ok := a.(Consonant)
	if !ok {
		return 0
	}
	cb, ok := b.(Consonant)
	if !ok {
		return 0
	}
	// mannerCoord runs high for obstruents, so a fall in it is a rise
	// in sonority.
	if mannerCoord[ca.Manner] > mannerCoord[cb.Manner] {
		return risingSonorityCost
	}
	return 0
}

// ocp is the similarity-avoidance arm, rebuilt. It vanishes for a
// geminate and peaks just above: the speaker found alla easier than
// alra, and the sources agree, since §1.7 permits geminates outright
// and §6 generates them, while what §2.4 and §2.5 bar is the near
// miss, two homologous consonants disagreeing in voicing. Avoidance
// applies to similar-but-distinct, not to identity.
func ocp(d float64) float64 {
	if d <= 0 {
		return 0
	}
	x := d / ocpPeak
	return ocpWeight * x * math.Exp(1-x)
}

const (
	// baseSegment is what any segment costs before its features.
	baseSegment = 0.05

	voicingCost        = 0.03
	roundingCost       = 0.03
	risingSonorityCost = 0.04

	// rhoticCost is charged on r and again, by half, on ř.
	rhoticCost = 0.04

	// obstruentGeminateCost is a held constriction. Above the sonorant
	// geminate, which pays only the vanishing similarity term, and
	// below anything §2 bars, since §1.7 permits these.
	obstruentGeminateCost = 0.55

	// ocpWeight is the height of the similarity bump and ocpPeak the
	// distance at which it sits: near-misses, not identities.
	ocpWeight = 0.30
	ocpPeak   = 14.0
)
