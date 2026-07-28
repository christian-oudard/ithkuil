# Open Work

Known defects and unimplemented features in this repository. The
inverse list, defects in the published sources rather than in our
handling of them, is `language_reference/issues.md`.

Each entry says what the evidence is, so that a later reader can tell a
measured gap from a guess. Corpus counts come from
`tools/discord_archive/fidelity`, run over the extracted study-group
archive. As of the last run: 3633 candidate words, 91.0% classified,
and of the 2932 that are formatives, 100.0% round-trip lossless, 99.6%
rendering to a legal word, 55.9% spelled exactly as attested.

## Unimplemented features

### 1. Case-accessor, inverse case-accessor and case-stacking affixes

§3.9.2. The V_X portion uses the first *four* standard vowel series;
our §3.7 table stops at three, so we reject the whole family. Eight
C_S increments carry it: **sw**, **sy** (case-accessor), **čw**, **čy**
(inverse), **šw**, **šy** (Type-3), **lw**, **ly** (case-stacking).
The first of each pair covers cases 1-36, the second 37-68, and the
V_X series picks the group of nine within that range.

Attested 26 times, and the evidence is clean: every series-4 V_X in the
corpus that sits on one of those eight clusters, and no other affix in
the corpus uses them at all. `ao+ly` x6, `eö+lw` x4, `ao+lw` x3,
`ao+sw` x3, `eö+sw` x2, `aö+sw` x2, then `sy` and `šw`.

Needs a new `grammar.Affix` variant carrying a case, plus gloss,
compose and serialize support.

### 2. The `üö` Ca-stacking affix

§3.5 and §3.7: the specialized V_X value **-üö** marks the following
C_S as a C_A complex stacked on the Slot VI C_A.

Attested 14 times, and every following C_S is a well-formed C_A, which
is what the rule requires: `üö+v` x4, `üö+d` x2, `üö+r` x2, `üö+s` x2,
`üö+ţ` x2, `üö+g`, `üö+j`. One of them, *maţřëullaitäzwüöra*, is built
on this project's own canonical test word.

**Blocked on a decision:** how a stacked C_A is written in the
canonical gloss. That is a SPEC.md question, not an implementation
detail, and it should be settled before the code is written.

## Open questions

These need an answer from the community or from Quijada. Do not guess a
rule for them; a wrong rule is worse than a rejected word, because it
silently mis-glosses.

### 3. Series-4 V_X on an ordinary C_S

79 attested occurrences over 39 distinct pairs: `eö+l` x12, `eö+ř` x8,
`oe+s` x4, `öa+s` x4, `öa+n` x3, `oa+ň` x3, `oa+ẓ` x3, and a long tail.

It matches nothing we can find. Not §3.9.2, whose C_S increments are
different. Not Ca-stacking, whose V_X is fixed at `üö`.
`affixes_reference.md` has no fourth series. The surrounding slot
layouts parse cleanly, so this is not us mis-segmenting the word.

It is the largest single unexplained bucket in the corpus, and 39
distinct pairs is too widespread to be typos.

### 4. The `ţḑ` C_A

§3.6's substitution table produces `ţḑ`, which no rule then resolves.
`allomorph.UnresolvedCa` documents the state in full, including two
hypotheses already refuted. DPL+A+RPV is attested zero times in either
corpus, so usage cannot settle it.

## Known defects

### 0. `üö` is missing from the vowel-conjunct table

`validation.validDisyllabicConjuncts` holds `üo` but not `üö`, so
`ValidateWord` rejects any word containing it. The Slot IV table gives
both (`| 0 | ae | ea | üo | üö |`), and `üö` is legal in two places:
the AMG degree-0 V_R of a specialized C_S-root, and the Ca-stacking
V_X. We therefore reject a degree-0 AMG C_S-root word today.

Found by hand-assembling a Ca-stacking word and running it through our
own validator. One-line fix, and a prerequisite for item 2.

### 5. Ultimate stress on a concatenated formative

§3.1.3: it ends in an alternate V_F, not a V_K. We read a V_K and
reject the word. 2 corpus words. Test skipped at
`fullparse/corpus_regression_test.go`.

### 6. A Slot VII C_S read as beginning with a glottal

§1.5 does not allow it: a glottal between a vowel and a consonant is
syllable-final, so it belongs to the V_X before it. The renderer drops
it and the affix reappears in Slot V a degree off. 4 corpus words.
Test skipped in the same file.

### 7. A lone C_C marker outside a hyphenated chain

14 corpus words read as formatives carrying a concatenation marker with
nothing to concatenate to. `serialize` refuses to encode them, since
the decoder would swallow the next token into a chain that never
terminates. Test skipped at `tokenize/corpus_test.go`, which also
explains why simply rejecting a lone C_C is not the fix: *hnas* is
meant to be a Naming carrier, and the formative reading is what
`isCarrierToken` currently leans on.

This is also where two of the twelve illegal renders come from,
*hoňkoň* and *höňkoň*.

### 8. Chains whose links each parse but whose chain is rejected

3 corpus words parse link by link and are then refused as a chain, so
they report no error at all in the audit's triage. Not yet diagnosed.

## Testing gaps

- **Whole-lexicon sweep.** Every root and affix in `data.db`, built
  into a formative, rendered, validated, and parsed back. The fuzz
  covers slot combinations against a handful of hand-picked roots; this
  would cover the lexicon against a handful of slot combinations.
- **§6 gemination in the spec-example table.** `validation/spec_examples_test.go`
  covers §2.2-§2.24, §3.1, §3.2 and §4.1, but not the §6 forms
  (`*ksff`, `*krrw`).
- **Differential testing against `IthkuilGloss`** (Kotlin). Blocked:
  not cloned in this environment.

## Not defects

Recorded so they are not investigated twice.

- **English written in Ithkuil letters.** *sërvër*, *drägën*, *mädnës*,
  *singël*, *tokyo-lëndën*, *iňgliš*, *webtüwns*. About 20 of the V_X
  failures and most of the remaining illegal renders. These are carrier
  passthrough, not Ithkuil.
- **Spelled-as-attested at 55.9%.** The renderer canonicalizes and the
  grammar permits several spellings of one word, so a mismatch here is
  a style difference. Only the lossless and legal-word figures are
  defect measures.
