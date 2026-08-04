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

The corpus is the calibration target. A cost model that cannot
reproduce Quijada's choices over the 583 distinct corpus words is
wrong, and each disagreement is either a rule we have misread or a
place the source is genuinely free.

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
