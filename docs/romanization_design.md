# Two Romanized Forms, and the Choice Between Them

A `grammar.Formative` does not determine one spelling. The grammar
offers a set of optional shortenings, and every one of them produces a
legal word with the same meaning:

```
mlaläh    ml-PRL
mlaläha   ml-PRL          same grammar, Slot IX default restored
mlaläho   ml-PRL-ERG      different grammar
```

Today `slots.FromGrammar` picks one of these while translating grammar
into slots, so the choice and the thing chosen are the same function.
This document separates them.

## The two forms

**Explicit form.** Every slot in its place, every default written, no
optional shortening taken. A structural image of the `Formative`. It is
a function of one word and nothing else, and no rule fires while
producing it. `roman.Formative` should return this.

**Spoken form.** The optional shortenings applied, subject to the
phonotactic rules that span word boundaries. It is a function of a
span, not a word, because §1.5 makes the decision depend on the
neighbour. `roman.Text` should return this.

The asymmetry is the design. Structure is per word, so the explicit
form is per word. Elision is per position, so the spoken form is per
span.

Put another way, the two forms sit on the two scales §2 names. The
explicit form is the clearest one, and it is clear by construction
rather than by scoring: nothing is elided, no shortcut is taken, no
glottal stop is moved, so every distinction the grammar draws is
written. The spoken form is the cheapest one to say. Neither needs a
metric to be defined; a metric is needed only to choose among the
spoken form's candidates.

That places the whole clarity-against-effort tradeoff inside the spoken
form, and nowhere else. §3.9.1's relocation of a case glottal stop is
the example: moving it can shorten the word and can also put the case
marker where it will not be heard. So the ranking has to know which
segments carry information, not merely which are hard to say. A
distinction lost is not a cost the effort model can see.

Both are deterministic. This is not an orthographic option offered to a
caller: one `Formative` has one explicit form, one `Text` has one
spoken form.

## Layout is the decision record

`slots.Layout` is already the pre-romanized formalism, and it already
carries decisions as fields rather than deriving them:

- `MovedGlottal` records whether the §3.9.1 shortening put the case
  glottal stop on an earlier vowel-form instead of on V_C.
- `CnInCa` records whether the §3.8.1.2 shortening wrote a Pattern-1
  Mood/Case-Scope C_N in the Ca slot.
- `Vc` empty versus `"a"` records whether the Slot IX default was
  elided.
- `Kind` and `Cc` record whether the §3.2 Slot IV/VI shortcut was
  taken.
- `Vn` and `Cn` empty record Slot VIII zero-marking.
- `Stress` records the prosodic stress to apply.

So the intermediate representation exists and is adequate. What changes
is where it gets filled in: `FromGrammar` produces the explicit Layout,
and a separate reducer turns that into the spoken Layout.

```
Formative  --FromGrammar-->  explicit Layout
                                   |
                              reduce (this document)
                                   v
                              spoken Layout  --Render-->  conjuncts
```

## Mandatory and optional

Not every shortening is a choice. Some the source states as
unconditional, and those belong in the explicit form because declining
them would produce a word the grammar does not admit.

§3.8.1.2 states one as unconditional:

> Note that the C_N—**Pattern 1** affix FAC/CCN -**h**- never moves to
> Slot VI because it instead elides whenever Slot VIII is zero-marked.

"Whenever" leaves nothing to decide. By contrast §3.1.3 states one as
available:

> Both Case No. 1 (THM) and Case No. 37 (PRN) can elide their -**a**-
> phonological marker […] Before eliding the -**a**-, External Juncture
> requirements of Sec. 1.5 must be taken into account.

"Can elide", and conditioned on juncture. That one is the reducer's.

Classifying each shortening against the source is the first piece of
work, and it is research rather than judgment. The candidates are the
Slot IX V_C and V_K defaults, the Slot II V_V default, the §3.2 Slot
IV/VI Ca shortcut, the §3.8.1.2 C_N-to-Slot-VI move, and the §3.9.1
glottal relocation for cases 37 through 52.

## Interior and edge

Among the optional shortenings, only some can matter to a neighbour.
The rules that span a boundary read word edges and nothing else:

- §1.5 (morphology) reads the first word's final consonant-form and the
  second word's initial consonant-form.
- §1.6 (phonotactics) reads the first word's last segment and the
  second word's first.
- §7.1 reads the consonant cluster spanning the seam; §7.2 reads a
  geminate on either side of it.

Nothing across a boundary depends on where a word put its glottal stop
or whether it moved C_N into Slot VI. So:

**Interior decisions** are settled on one Layout with no context. The
§3.9.1 glottal relocation, the §3.8.1.2 C_N move, the §3.2 shortcut and
the Slot II default are interior.

**Edge decisions** are the only ones the span-level pass needs to see.
Filling or eliding the Slot IX vowel is the main one, because it
decides whether the word ends in a vowel or a consonant.

That keeps the span-level pass small. Each word offers its legal
spellings in preference order and the pass picks a combination that
satisfies the boundary rules. `roman/referential.go` already works this
way within a word: it builds candidate spellings and `pickValid` takes
the first that `phonology.Legal` accepts. The span-level pass is the
same shape one level up, and it works on strings plus a short candidate
list, so word classes without a Layout (referentials, and the bias,
register, carrier, modular and parsing adjuncts) need no new machinery.

## Why filling the Slot IX default is usually right

§1.5 of the morphology is the governing rule:

> When a word ending in a consonant-form […] is followed in the same
> breath-group by another word beginning with a consonant-form, it is
> usually necessary to append a vowel either to the end of the first
> word or the beginning of the second word, so as to avoid confusion as
> to which word the word-final and/or word-initial consonants belong
> to. This is accomplished by ensuring that appropriate word-initial
> and/or word-final vocalic Slots (e.g., Slot II, Slot IX) are filled.

Since §1.2 of the phonotactics makes every word consonant-initial, the
antecedent holds at every junction after a consonant-final word. The
corpus bears this out. Of 467 junctions inside a clause, 4 put a
consonant before a consonant, and all four are before `w-`. A word ends
in a consonant 4.3% of the time clause-medially and 15.5% clause-
finally.

The same corpus shows the Slot IX default is usually written:

```
Slot IX written "a" (THM):   220
Slot IX empty (THM elided):   65
Slot IX some other vowel:    238
```

and the 65 elisions cluster in paradigm lists such as `anzwul anzwut
anzwuk anzwup anzwuf anzwuç anzwuž`, which are citation forms in a
demonstration of word-final consonants rather than running text.

The remedy costs nothing, because the vowel is not epenthetic in the
arbitrary sense. It is the default the elision dropped, so restoring it
cannot change the reading. Only a different vowel can.

## The cost model

Where the source leaves genuine slack, something has to choose. §1.5
offers the vowel at either end. §7.1 offers three remedies. §1.5 says
"usually necessary" rather than "necessary".

The choice is made by a cost model, under one constraint:

**The cost model may only rank forms the rules already permit.** It
never makes an illegal form legal, and it never rejects a legal one. It
is a selector over a candidate set, not a validator. A wrong cost model
therefore produces a clumsy word, never an ungrammatical one, and the
correctness path does not run through it.

Its terms are the source's own stated reasons rather than invented
phonetics. §2 of the phonotactics names two in its opening sentence:

> Due to difficulty/awkwardness in pronunciation, or because they are
> too phonetically indistinguishable from other forms, the following
> general restrictions on consonantal forms apply

and §1.5 of the morphology names the third, "so as to avoid confusion
as to which word the word-final and/or word-initial consonants belong
to". Articulatory difficulty, confusability with another form, and
boundary ambiguity. §1.2.2 supplies concrete instances of the middle
term: `ļ` and `hl` are allophonically identical, which is why §5.1 bars
`ļ` between vowels, and `př`/`tř` need care against `px`/`tx`.

The corpus is one calibration target. A cost model that cannot
reproduce Quijada's choices over the 583 distinct corpus words is
wrong, and each disagreement is either a rule we have misread or a
place the source is genuinely free.

### Articulatory effort

The three terms above are reasons, not a metric. The metric is a sum of
transition costs over the segment sequence, and the features it needs
are already modelled. `phonology.Consonant` carries `Voicing`, `Place`
and `Manner`; `phonology.Vowel` carries `Height`, `Backness` and
`Rounding`. These are the same features §1.1 tabulates and the same
ones Quijada's own rules reason over: §2.4, §2.5 and §2.13 turn on
"homologous", defined in §0 as sharing a place of articulation, and
`phonotactics.go` already implements them through `areHomologous`.

Three terms are available without adding anything:

- **Voicing switches.** The count of adjacent segments differing in
  `Voicing`. Each is a laryngeal reconfiguration. §2.4 and §2.5 forbid
  the switch outright at a shared place of articulation, so a gradient
  penalty generalises a judgment the source already makes.
- **Sonority profile.** `Manner` orders as stop, affricate, fricative,
  nasal, lateral, approximant, vowel. Rising into a nucleus and falling
  out of it is the unmarked shape; departures cost. The ordering is a
  choice among competing published scales and should be recorded as
  such rather than presented as a fact.
- **Place distance.** The `Place` enum runs front to back, Labial
  through Glottal, so the gap between adjacent segments approximates
  articulator travel. That the enum is anatomically ordered is
  currently an accident of how it was written; anything depending on it
  must say so, and the ordering must then be held by a test.

A tongue-shape term needs one thing the model does not yet have: the
active articulator (tip, blade, body, root) as distinct from the
passive target `Place` records. Two consonants made with different
organs can be coarticulated at no travel cost, while two made with the
same organ at different places require real movement, and nothing in
the current features distinguishes those cases.

Double articulation is handled, though, since it was the one place the
inventory disagreed with §1.1. `w` is now `Velar` plus `Labialized`
rather than plain `Labial`, so it sits beside `u` rather than
equidistant from `u` and `i`. `ř` is an `Approximant` rather than a
`Trill`, per §1.2.2, which gives the trill as its geminate allophone.

Beyond that lies gestural overlap and coarticulation, which is not a
matter of reading a feature off a table but of adopting a phonological
theory. That is out of scope until the cheap terms are shown to be
insufficient.

### Two place models

`phonology` currently holds place of articulation twice, and the two
disagree. `inventory.go` has the typed `Place` enum, and
`phonotactics.go` has `placeGroup`, a rune switch into six groups, and
it is `placeGroup` that `areHomologous` and therefore §2.4, §2.5 and
§2.13 actually consult.

They part over the front coronals. `placeGroup` puts `t d ţ ḑ n`
together and `s z c ẓ` apart, following §1.1's Apico-dental and
Apico-alveolar columns. `inventory.Place` puts `t d n` at `Alveolar`
with `s z`, and `ţ ḑ` at `Dental`, following the IPA reading of [θ ð].

An effort table built on `inventory.Place` would therefore disagree
with the rules about which segments share a place. These must become
one model before the table is built, and the source table should win,
since the rules being implemented are Quijada's. `section8_test.go`
regenerates §8's 810 cells from the pair rules and is the instrument
for making the change safely: unify the two, and it says whether the
grid still comes out.

### The transition table

The feature terms compile down to a table: the cost of moving from one
phoneme to the next. With 31 consonants and 9 vowels that is 1600
ordered pairs, small enough to pre-generate the way `allomorph`
pre-generates its Ca forms.

The table is indexed by three things, not two: the pair, and whether it
spans a word boundary. The same pair can differ. `hh` is a permissible
geminate inside a word, `ClusterLegal("hh")` accepts it, and §1.6
forbids it across a boundary. So the boundary is a modifier on the
transition rather than a segment in the sequence, and §1.5, §1.6, §7.1
and §7.2 all become entries in the boundary half of the table instead
of four special cases in the reducer.

A word's energy is the sum over its adjacent pairs. A span's energy is
the sum over its words plus the boundary transitions between them, so
within a word and across words are one mechanism.

### Where infinity comes from

Not from the table. The categorical rules are not pairwise and a
pairwise table should not try to express them:

- §2.13 bans a nasal plus homologous stop plus sibilant, and says why:
  \***nks** is "too phonetically indistinguishable" from **ňs**. The
  cost is a property of the trigram, not of any pair inside it.
- §2.19 constrains -**h**- only as the final member of a conjunct,
  which is a position rather than a pair.
- §4's word-final tables and §§9 through 11's conjunct tables judge
  whole clusters.

So legality stays where it already lives, in `phonology.Legal` and
`ClusterLegal`, and supplies the infinity. The table ranks only what
survives that. This makes the constraint above structural rather than a
rule to remember: the ranking function has no way to reach an illegal
form, because illegal forms are removed before it runs.

If every candidate for a span is infinite, the reducer fails rather
than emitting the least bad one. `roman.pickValid` already has that
shape.

### Minimizing

Energy is a sum over adjacent pairs, so a span is a linear chain and
the minimum is a shortest path, not a search over the product of every
word's choices. Viterbi over N words with K candidate spellings each
costs O(N·K²).

K stays small because interior decisions cannot affect a neighbour, by
the argument above. Minimize those locally per word first; the chain
then ranges only over edge-affecting variants, principally whether the
Slot IX vowel is written.

### Prior art

The model is not novel, which is a good sign and a source of parts.

**Lindblom's H&H theory** (Hyper- and Hypo-articulation, 1990) is the
same shape: a speaker minimizes articulatory effort subject to
sufficient discriminability, hypo-articulating only as far as the
listener can still tell the word from its competitors. That is our
minimization with `phonology.Legal` in the role of the discriminability
floor, and it is worth knowing that the floor being categorical here,
where Lindblom's is gradient, is a simplification we are choosing.

**Kirchner's effort-based account of lenition** (1998) supplies the
place-distance term and its justification: an articulation is more
effortful the further and the faster the articulators travel. It also
warns about a case we will meet, geminates, which resist reduction
because holding a constriction is itself costly. Ithkuil permits
geminates (§1.7) and generates them (§3.6.1 Ca gemination), so
"repeated segment is free" would be the wrong default.

**Boersma's Functional Phonology** (1998) formalizes articulatory ease
and perceptual confusion as separate competing drives. That is two of
the three terms §2 names, kept apart rather than summed into one
number, which is the right instinct: a cluster can be easy to say and
still be barred for sounding like something else, which is exactly
§2.13's reason for rejecting \***nks**.

**Kondrak's ALINE** is the closest ready-made component. It decomposes
each phoneme into multivalued articulatory features, place and manner
for consonants, height and backness for vowels, which is the
decomposition `phonology.Consonant` and `phonology.Vowel` already use,
and weights each feature by a hand-set salience to yield a distance
between segments. Its salience weights are a published starting point
for ours, and its use of hand-set rather than fitted weights is the
practice recommended below.

**Unit-selection speech synthesis** already runs the algorithm. A join
cost scores each pair of adjacent units, a target cost scores each unit
against what was wanted, and Viterbi finds the sequence minimizing the
total. Our transition table is a join cost and our candidate spellings
are the unit inventory. One reported practical finding transfers
directly: the number of join costs actually needed is a small fraction
of all possible pairs, so a sparse table with a principled default is
likely to be enough.

### Numbers to start from

ALINE publishes its whole parameter set, and its feature decomposition
is ours. Quoted from Kondrak (2000), Tables 3 and 4.

Multivalued features, as coordinates on [0,1]:

```
Place    bilabial 1.0   labiodental 0.95  dental 0.9   alveolar 0.85
         retroflex 0.8  palato-alveolar 0.75            palatal 0.7
         velar 0.6      uvular 0.5        pharyngeal 0.3  glottal 0.1
Manner   stop 1.0  affricate 0.9  fricative 0.8  approximant 0.6
         high vowel 0.4  mid vowel 0.2  low vowel 0.0
High     high 1.0  mid 0.5  low 0.0
Back     front 1.0  central 0.5  back 0.0
```

Feature saliences, the weights on each feature's contribution:

```
Manner 50   Place 40   Voice 10   Nasal 10   Lateral 10   Retroflex 10
Syllabic 5  Aspirated 5   High 5   Back 5   Round 5   Long 1
```

The segment distance is then `δ(p,q) = Σ_f diff(p,q,f) × salience(f)`.
Manner outranks place, and voicing is a quarter of place.

Two things to notice before adopting any of it.

**The place scale disagrees with our enum on one pair.** ALINE runs
alveolar 0.85, retroflex 0.8, palato-alveolar 0.75, so retroflex comes
before palato-alveolar; `Place` declares `PostAlveolar` before
`Retroflex`. It matters for us because §1.1 files **š ž č j** under an
"Alveolar Retroflex" column while `inventory.go` records them as
`PostAlveolar`. Whichever we keep, the enum order and the scale have to
be made to agree, and `TestPlaceIsOrderedFrontToBack` is where the
answer gets written down.

**ALINE measures similarity, not effort, and the two are not the same
sign.** It exists to align cognates, so a small δ means two segments
resemble each other. Adjacent segments that resemble each other are
penalized, not rewarded: that is the Obligatory Contour Principle, and
it is what almost every §2 rule turns out to be. §2.4 and §2.5 bar
homologous stops, fricatives and affricates that disagree in voicing;
§2.10 bars **ç** beside a sibilant; §2.13 bars a nasal plus homologous
stop plus sibilant for being "too phonetically indistinguishable" from
the same string without the stop. All of those are same-place
restrictions, none is about travel.

So effort is not monotonic in δ. It is U-shaped, and needs two terms of
opposite sign:

- a travel term rising with δ, which is Kirchner's, and which ALINE's
  place coordinates measure directly;
- a similarity term falling with δ, penalizing near-identity, which is
  the OCP and which §2 already encodes categorically.

Geminates sit at δ = 0 and are the test of whether the second term is
right. §1.7 permits them, §3.6.1 generates them, and §1.7 bars only the
triple, so the similarity penalty must be finite at zero rather than
prohibitive.

For manner, ALINE's scale is a sonority scale read backwards, stop 1.0
down to low vowel 0.0. It is too coarse for Ithkuil, which contrasts a
tap `r`, an approximant `ř`, a lateral approximant `l` and a lateral
fricative `ļ` that ALINE would treat with binary Lateral and Retroflex
flags. Parker's acoustically grounded scale separates them: seventeen
classes ordered low vowels, mid vowels, high vowels, ə, ɨ, glides,
rhotics, flaps, laterals, trills, nasals, voiced fricatives, voiced
affricates, voiced stops, voiceless fricatives and h, voiceless
affricates, voiceless stops and ʔ. The reference vowel [ɑ] indexes 17.
The per-class integers are an inference from seventeen classes counting
down from that reference, not a quotation, and should be checked
against Parker (2008) before being relied on.

### Calibrating the effort model

The corpus exhibits a handful of binary choices, so a model with
several free weights fitted to it would be unfalsifiable. Prefer few
terms with fixed, argued weights over many with fitted ones. A
tie-breaker that cannot be shown wrong is not worth having.

§2 is not a fit target either, since legality is decided before the
table runs and the table is never asked to reproduce it. It is still a
sanity check. §2's prohibitions are Quijada naming conjuncts as
difficult or confusable, and §8 renders that judgment over 810 cells.
A table that scored the prohibited pairs as cheap would be measuring
something other than effort, even though nothing depends on it
directly.

## Chains

A §3.1.7 concatenation chain is written with hyphens, but §3.1.8 calls
the hyphen "a simple mnemonic indicator", §3.1.6 gives each link its
own stress, and §3.1.3 subjects a link's elision to §1.5 by name. So
the links are separate prosodic words inside one breath group, and the
boundary rules apply at every hyphen.

Chains are the one place where the pause remedy is unavailable, since a
pause inside a chain is not a chain. They are also the one place where
no breath-group model is needed, because a chain is one breath group by
construction. A chain is therefore a span, and `roman.Word` on a
`*grammar.Chain` should route through the same span-level pass as
`roman.Text`.

## Pauses are not modelled

Every boundary rule is conditioned on the same breath group and offers
a pause as its escape. We do not represent either, because neither is
grammar. §5.8 ¶8 defines the unit ostensively, "an initial utterance or
an utterance preceded by a pause for breath", and §1.3.2 says why the
juncture markers are not written:

> these are normally never written in either the romanization scheme or
> the native New Ithkuil script, given that their occurrence is
> entirely dependent on the specific way any given individual utters a
> sentence or group of sentences on any particular occasion.

A pause is a fact about one performance. A `grammar.Text` holds
grammar, so it does not hold pauses. The spoken form's job is to need
no pause.

Two boundaries are fixed by rule rather than by performance and so can
be derived: a bias adjunct takes a pause on both sides (§5.8 ¶9), and a
foreign name before a carrier adjunct takes one after it (§4.5).

## Testing

Splitting the layers gives each one a test it can fail on its own.

- Explicit form: round trip. Reading the explicit form of every
  `inventory` sample returns the sample. No juncture, no corpus, no
  cost model.
- Reducer: corpus agreement. The spoken form of a parsed corpus
  sentence should be the sentence Quijada wrote.
- Cost model: the same corpus, read as a fit rather than as a pass or
  fail, so that a disagreement is reported with the rule it turns on.

Under the present single-layer arrangement these are one test, and it
cannot distinguish a structural error from a stylistic one.

## Open questions

**Is standalone Slot IX elision sourced?** The only explicit permission
to elide a THM `-a` is §3.1.3, which is stated for concatenated
formatives. The nearest support for standalone formatives is §3.9:
"If a word does not have sufficient syllables to take antepenultimate
stress, add syllables by filling Slots II and/or Slots VIII and IX with
their default values", which implies those slots are normally unfilled.
That is inference, not a rule. It decides whether `mlaläh` is a legal
spelling at all or only `mlaläha` is.

**What does filling cost a verbal formative?** The Slot IX V_C default
is written `(a)` in §3.9.1 and the V_K default `(á)` in §3.9.3.3. V_K
requires ultimate stress, so filling a verbal adds a syllable and a
diacritic where filling a nominal adds neither.

**Word-final bare `-h`.** §4.1 permits it, "any single consonant except
-**w** or -**y**", and §3.8.1.2 generates it whenever a non-default V_N
meets the default C_N with Slot IX elided. It appears in 37 of 294
`inventory` samples and in 0 of 583 corpus words. Whether that gap is
the juncture rules operating or a bare `-h` being avoided outright is
not settled by anything found so far.

## Order of work

1. Correct the effort model where speaker judgments already say it is
   wrong. It stands at 36 of 48. The rhotic term comes first: ř is dear
   for being a rhotic and not for being uvular, which the current
   `placeCost` gets backwards, and x is only "hard-ish". Then split
   geminates by sonorancy, since `alla` is easy while `assa` and `atta`
   are not: holding a constriction is the cost, which is Kirchner's own
   account of why geminates resist lenition.
2. Rank by energy instead of by hand-ordered legality. `pickValid` has
   three callers building real candidate sets and takes the first that
   can be said rather than the cheapest. That is the selection point,
   selecting on the wrong criterion, and wiring it is small. Do it after
   1, because wiring a model with known wrong answers propagates them.
3. Make elision conditional. Defaults are elided unconditionally today,
   and three slots have now turned up where the source conditions it on
   what elision produces: Slot IX against §1.5 juncture, and V_S and V_Z
   against §4.2. §3.1.3 states the condition outright. One policy, not
   three bugs.
4. Teach the ranking which segments are load-bearing, so §3.9.1 and the
   other information-carrying choices can be made on clarity rather than
   on effort.
5. Enforce §8 and §9 at the root and affix boundary. No such boundary
   exists in the code yet, and the §9 table needs embedding rather than
   parsing from this directory at runtime.
