# Translating Literature

Ithkuil makes a translator commit. Several categories are mandatory and
have no unmarked value, so a passage that leaves something open in
English or Japanese cannot stay open here: the slot has to be filled,
and every value in it is a claim. Translating a poem is therefore an act
of criticism, and the reading ends up visible in the morphology whether
the translator meant to publish it or not.

This file is about that problem. `grammar_explanations.md` says how each
category lands in English; this one says which categories force an
interpretive decision, and what taking the default costs.

Nothing here is a claim about the language. It is a working method.

## No category abstains

The trap is that every mandatory slot has a value that feels neutral and
is not.

- **Context** must be `EXS`, `FNC`, `RPS` or `AMG`. `EXS` reads as the
  absence of a choice and is in fact the strongest ontological claim
  available: that the thing simply exists. A figurative reading is
  `RPS`; the refusal to choose is `AMG`.
- **Validation** attaches to every `ASR`. Leaving it off does not leave
  the question open, it asserts `OBS`: the speaker saw this. For a
  first-person narrator who cannot be trusted, that is a decision.
- **Illocution** must be present. `ASR` is not silence.

So the first pass over any literary text is a hunt for the places where
the source is deliberately withholding, and a decision about which
Ithkuil value carries the withholding.

## The categories that carry interpretation

**Context** is the big one, and `AMG` is the reason. A metaphor rendered
`RPS` says the speaker knows it is a metaphor. The same image rendered
`EXS` says it is literally so. `AMG` says both at once, which is what a
symbol usually is in a poem, and is the only value that declines
explicitly rather than by omission. Ambiguity by declaration is a thing
Ithkuil can do and most languages cannot. It is worth reaching for.

**Validation** is where an unreliable narrator lives. `IMA` marks a
claim as dream- or fantasy-sourced, `INF` as inferred, `ITU` as known by
feeling, `RPR` as reported. A narrator who cannot tell sleep from waking
should not be asserting `OBS` throughout; putting the unreliability in
the Validation makes it grammatical instead of merely thematic, and the
reader meets it before the content admits to it.

**Referents** can decline to identify. `PVS`, the provisional referent,
leaves identity unsettled. Where a text asks "who is watching?" and
means the question, `PVS` is the answer, and naming a specific referent
would resolve what the line holds open.

**Illocution** distinguishes kinds of asking. `IRG` asks; `VER` asks for
confirmation, which presupposes someone who could confirm. A question
addressed to an absent listener is more painful in `VER` than in `IRG`,
precisely because `VER` presumes the listener is there. `HOR` is "I wish
it were so", counterfactual from inside the wish, and is purpose-built
for the if-only line most poems have somewhere.

**Level** grades against a norm, and is the natural home for the
extent adjectives that literary text is full of: long, deep, endless,
heavy. Reaching for a degree-9 affix instead is a common reflex and
usually the weaker choice, since it spends an affix slot on something a
Slot VIII value already says.

**Phase** covers recurrence: `REP`, `ITR`, `ITM`, `FRE`. A cycle built
out of several affixes is usually a Phase value that was not found.

## What the grammar will not do

Ithkuil is good at ambiguity it has a name for and clumsy at ambiguity
it does not.

It can encode that an action recurs. It cannot, in the same breath, say
that the speaker chose the recurrence, which is often the whole point of
a line about a cycle. It can name two referents, but naming them makes
them two, so a speaker who is split between two selves gets resolved by
the act of description. Wanting two incompatible things at once has no
slot.

When this happens, the honest move is to let the affixes carry it and
accept that the grammar is silent, rather than to pick a category that
approximates and quietly changes the claim.

## Traps

**Mood becomes Case-Scope without an illocution.** Slot VIII Cn is Mood
in a verbal formative and Case-Scope in a nominal one, and nothing warns
you:

    S1.PRC-dzgr-COU      -> ẓgrahna -> parses back as  dzgr-CCP
    S1.PRC-dzgr-COU-ASR  -> ẓgrahn  -> parses back as  dzgr-COU-ASR-ULT

A mood only survives on a word that is verbal. This produces a different
grammatical category rather than an error, so it fails silently.

**Stems carry meaning that is easy to spend an affix on.** A root's four
stems often already hold the distinction being reached for. Check the
stems before adding an affix.

**Defaults flatten.** A translation that never uses Level, Phase, Mood
or Validation is not neutral, it is a translation in which every
assertion is a directly observed, unmodalised, non-recurring fact. Run
the finished text through `ithkuil parse --short` and count which
categories never appear. An empty column is a question, not necessarily
a defect.

## Recording a stance

Write the decisions down before translating, as rules rather than as
line notes: which imagery takes `AMG`, which content takes `IMA`, where
the extent adjectives go. A stance stated up front can be applied
consistently and argued with. Decisions taken line by line cannot, and
tend to drift toward the default.

Then check every word round-trips. `ithkuil compose` on the gloss and
`ithkuil parse --short` on the result should return the gloss you
started from; where they do not, the word is not saying what the gloss
claims.
