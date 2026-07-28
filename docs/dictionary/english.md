# English to Ithkuil

The mapping from English into Ithkuil, written as the definition
language underneath English. Authored by hand rather than generated.
Other languages get their own file beside this one.

## What earns an entry

Words are worked through in frequency order, from
`common_words_50k`. Two kinds are left out.

Grammatical words are skipped. *The*, *of*, *not*, *a*, *any* are not
lexical in Ithkuil at all: they are cases, affixes, or nothing. *The
fruit* against *a fruit* against *any fruit* is one root under three
grammatical settings, and explaining that belongs in a grammar guide
rather than in a list of words. Words that look lexical but are
grammatical in disguise, *thing* and *one* and *someone*, go with them.

Specialist vocabulary is also skipped. The lexicon already names some
14,000 species and compounds in English, and `ithkuil define trout`
finds them without help. This file is for the words common enough that
the useful question is not whether a root exists but which of several
readings to take.

## How to read an entry

Each line gives the gloss expression, the word it composes to, and the
English reading. The gloss expression is the authoritative half: it is
what `ithkuil compose` reads, so every line here can be checked.

    $ ithkuil compose "S2-l"
    elal   S2/PRC--l-'human child'

Every form below was composed that way, and every phrase was read back
with `ithkuil analyze` to confirm it parses to what it claims. The gloss
expression, not the English word, is the identity of an entry: when
other languages arrive they attach to the same expression, and English
becomes one label among several rather than the key everything hangs
off.

Stems are written S1, S2, S3, and S0 for the general reading that covers
all three. `DYN` makes the word verb-like, an act rather than an entity.
`ASR` makes it a verb proper, an assertion. `G` is Agglomerative
perspective, which is where English mass nouns live.

---

## person, people

`-l-` is the human being. The stem carries the age, which English splits
across separate words.

- `S0-l` → **olal** — a human being, age not at issue
- `S1-l` → **lala** — an adult
- `S2-l` → **elal** — a child
- `S3-l` → **ulal** — an adolescent

English *people* is not a word here, and it is worth being careful about
which sense is meant. For "one or more humans, the number being beside
the point", use Agglomerative perspective:

- `S1-l-G` → **lara** — adults, any number of them

For a true plural, "two or more", §3.6 is explicit that Agglomerative
does not do it and the **XX2** affix at degree 6 does:

- `S1-l-XX2/6` → **lalöks** — two or more adults

*Man* and *woman* are not stems of this root. Sex is not part of the
lexical identity of a human in Ithkuil, and asking for *man* the way one
asks for *child* is the wrong shape of question.

## water

`-ţr-` water. The stems separate the sources.

- `S1-ţr` → **ţrala** — water
- `S2-ţr` → **eţral** — natural fresh water
- `S3-ţr` → **uţral** — artificial fresh water

The important thing for an English speaker is that the plain form is a
count noun. §3.6 uses this exact word as its example: Monadic **ţrala**
is *a drop of water*, a single unit of it, and the mass noun English
reaches for by default is the Agglomerative.

- `S1-ţr-G` → **ţrara** — water, some water, an amount of water

Wanting *water* and writing **ţrala** gets *a drop*. This trap is
general: rice, hair, sand, money all behave the same way.

## eat, drink

`-tx-` is nutritional consumption, one root where English has two verbs.

- `S1-tx` → **txala** — consuming
- `S2-tx` → **etxal** — eating food
- `S3-tx` → **utxal** — drinking liquid

As verbs:

- `S2-tx-DYN-ASR` → **etxulá** — eats, is eating
- `S3-tx-DYN-ASR` → **utxulá** — drinks, is drinking

In use, with the drinker as Ergative and what is drunk as Absolutive:

    utxulá welo ţrare
    The child drinks water.

## give, receive

`-n-` is the transfer of possession itself. English needs two verbs
because it names the transfer from each end; Ithkuil names the transfer
and lets the stem pick the end.

- `S1-n` → **nala** — a transfer of possession
- `S2-n` → **enal** — giving
- `S3-n` → **unal** — receiving

As verbs:

- `S2-n-DYN-ASR` → **enulá** — gives
- `S3-n-DYN-ASR` → **unulá** — receives

This is the shape to expect for many English verb pairs. *Buy* and
*sell*, *teach* and *learn*, *lend* and *borrow* are each one event seen
from two ends, and the language treats them that way.

## see

`-ẓ-` vision.

- `S0-ẓ` → **oẓal** — sight, vision
- `S1-ẓ` → **ẓala** — sight, the sense
- `S2-ẓ` → **eẓal** — an eye, the organ
- `S3-ẓ` → **uẓal** — one's visual faculty
- `S1-ẓ-DYN-ASR` → **ẓul** — sees

Note that the organ is a stem of the same root as the sense. Ithkuil
groups faculty, organ, and capacity under one lexical identity where
English has *sight* and *eye* as unrelated words.

## say, speak

`-m-` is linguistic utterance for communication.

- `S1-m` → **mala** — an utterance
- `S1-m-DYN-ASR` → **mul** — speaks, says something

## know

`-ţt-` knowing, and the stems sort it by how the knowledge was got, a
distinction English does not mark at all.

- `S0-ţt` → **oţtal** — fact, knowing
- `S1-ţt` → **ţtala** — knowing an ontological fact, that something is so
- `S2-ţt` → **eţtal** — knowing an epistemological fact, knowing it as knowledge
- `S3-ţt` → **uţtal** — knowing from experience
- `S1-ţt-DYN-ASR` → **ţtul** — knows

## think

`-sl-` thought.

- `S0-sl` → **oslal** — thought, idea, concept
- `S1-sl` → **slala** — thinking, cogitation
- `S2-sl` → **eslal** — consideration, mulling
- `S3-sl` → **uslal** — reasoning
- `S1-sl-DYN-ASR` → **slul** — thinks

English *think* also carries "believe" and "have an opinion", which is a
different notion and does not belong to this root.

## make

`-ţk-` making, and the stems distinguish how the thing is made.

- `S0-ţk` → **oţkal** — make, construct, form
- `S1-ţk` → **ţkala** — making, construction
- `S2-ţk` → **eţkal** — forming by combining ingredients
- `S3-ţk` → **uţkal** — organizing disparate parts into a whole
- `S1-ţk-DYN-ASR` → **ţkul** — makes

## work

`-rtm-` work, labor.

- `S0-rtm` → **ortmal** — work, labor, job
- `S1-rtm` → **artmal** — work, labor
- `S1-rtm-DYN-ASR` → **artmulá** — works

## time

English *time* is several unrelated notions, and they take different
roots.

Elapsed time, duration:

- `S0-gẓ` → **ogẓal** — a temporal interval
- `S1-gẓ` → **gẓala** — a degree of elapsed time

Time of day:

- `S1-ln` → **alnal** — daytime
- `S2-ln` → **elnal** — nighttime
- `S3-ln` → **ulnal** — o'clock time, the time on a clock

Asking for "the word for time" has no answer. Which of these is meant
has to be settled before the sentence can be built, and that is the
normal condition of translating into Ithkuil.

## day, night, year

- `S1-ř` → **řala** — a day, a 24-hour period
- `S1-ln` → **alnal** — daytime, the light part
- `S2-ln` → **elnal** — nighttime

`-rn-` covers the longer calendrical periods.

- `S1-rn` → **arnal** — a calendar year
- `S2-rn` → **ernal** — a decade
- `S3-rn` → **urnal** — a lifetime, an expected span

*Day* meaning the 24-hour unit and *day* meaning the daylight hours are
different roots, and English runs them together.

## place

`-ţkl-` place.

- `S0-ţkl` → **oţklal** — setting, place, site, venue
- `S1-ţkl` → **aţklal** — a setting
- `S2-ţkl` → **eţklal** — a place, a site

## house, home

Two roots, and English blurs them.

`-rm-` is the physical dwelling.

- `S1-rm` → **armal** — a dwelling, where an entity lives
- `S2-rm` → **ermal** — a domicile, an artificial dwelling for protection
- `S3-rm` → **urmal** — a house, a constructed residence with conveniences

`-nkr-` is one's surroundings, and *home* in the sense of where one
belongs.

- `S1-nkr` → **ankral** — home
- `S2-nkr` → **enkral** — environmental circumstances
- `S3-nkr` → **unkral** — the world

## world

`S3-nkr` → **unkral**. The world is the third stem of the surroundings
root, the outermost ring of *home*. There is no separate word for it.

## life

`-šw-` the living.

- `S0-šw` → **ošwal** — a lifeform
- `S1-šw` → **šwala** — a living being, a lifeform
- `S2-šw` → **ešwal** — vitality, activity consistent with being alive
- `S3-šw` → **ušwal** — being brought to life, biological reproduction

English *life* also means "a lifetime" (`S3-rn` → **urnal**) and "the
way one lives", which is a different root again. The three are not
interchangeable.

## money

`-šč-` money and financial transaction.

- `S0-šč` → **oščal** — money, financial transaction, banking
- `S1-šč` → **ščala** — a medium of exchange
- `S2-šč` → **eščal** — money, currency
- `S3-šč` → **uščal** — a quasi-contractual document

Money is a mass noun in English, so `S2-šč-G` is usually wanted over the
bare **eščal**, on the same grounds as *water*.
