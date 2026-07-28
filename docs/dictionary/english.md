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
with `ithkuil parse` to confirm it parses to what it claims. The gloss
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

## day, week, month, year

Two roots carry the calendar between them, split at the year.

`-ř-` is the conventionalized time period, the ones a body or a moon
gives you.

- `S1-ř` → **řala** — a day, a 24-hour period
- `S2-ř` → **eřal** — a week
- `S3-ř` → **uřal** — a month, a lunar period

`-rn-` is the longer periods.

- `S1-rn` → **arnal** — a calendar year
- `S2-rn` → **ernal** — a decade
- `S3-rn` → **urnal** — a lifetime, an expected span

*Day* meaning the 24-hour unit and *day* meaning the daylight hours are
different roots, and English runs them together. Daytime is `-ln-`,
under *time* above.

`-rḑ-` is the same series again but designated: `S1-rḑ` → **arḑal**, a
particular day on a calendar rather than a day as a span.

## night

- `S2-ln` → **elnal** — nighttime

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

---

## Adjectives are usually affixes

Most of the English adjectives near the top of the frequency list are
not roots at all. They are degrees on a nine-point affix scale, attached
to whatever they describe. Looking for a root meaning *big* is looking
for the wrong thing.

**SIZ** `-x-`, degree of size, is the clearest case, and it gets a
worked entry of its own under *big, large* below.

**QUA** `-th-`, degree of quality, effectiveness, or adequacy. English
*good* and *bad* are two points on it.

- `S1-l-QUA/6` → **lalöth** — a good adult, one who is effective at it
- `S1-l-QUA/3` → **laleth** — a bad, poor, inadequate one

Degree 5 is *not bad, adequate*; degree 8 is *excellent*; degree 9 is
*too good*. Choosing among nine degrees is the normal cost of saying
*good* in Ithkuil.

**NEW** `-sp-`, newness and revision, and the degrees are distinctions
English does not draw. Degree 1 is never before seen at all; degree 2 is
new only within the present context, the sense in *a new student*.

- `S1-l-NEW/1` → **lalasp** — an adult never before seen
- `S1-l-NEW/2` → **laläsp** — an adult new to this setting

**MDN** `-nţ-`, degree of age or modernity, covers *old* in the sense of
era: degree 3 ancient, 4 old as in first, 5 old as in former, 6 modern.

- `S1-l-MDN/3` → **lalenţ** — an ancient adult

*Old* meaning aged is not this affix. A person's age is carried by the
stems of `-l-` under *person* above, and old age proper is `-dç-`.

## man, woman, boy, girl

Sex is the **SEX** affix `-š-`, not a stem and not a root. Degree 1 is
female, degree 3 male; 5 is intersex, 6 unknown, 9 sex-neutral.

- `S1-l-SEX/3` → **laleš** — a man
- `S1-l-SEX/1` → **lalaš** — a woman
- `S2-l-SEX/3` → **weleš** — a boy
- `S2-l-SEX/1` → **welaš** — a girl

The four English words are one root, one stem choice, and one affix
degree. Note that nothing forces the affix: **lala** is an adult whose
sex is not at issue, which is not the same as degree 9, an adult whose
sex is explicitly beside the point.

`-š-` is also a root in its own right, where sex itself is the subject:
`S1-š` → **šala**, biological sex.

## walk, run, go

`-g-` is ambulation, sorted by manner rather than by direction. English
*go* is not here at all: it says that motion happened without saying how,
which this root will not do.

- `S0-g` → **ogal** — ambulation
- `S1-g` → **gala** — natural ambulation, walking or crawling
- `S2-g` → **egal** — rapid ambulation, running or galloping
- `S3-g` → **ugal** — unnatural ambulation, limping or staggering
- `S1-g-DYN-ASR` → **gul** — walks

Whether it is walking or crawling depends on the creature, not on the
word. The stem names the relation of the gait to the body that has it.

## want

`-gv-` desire.

- `S0-gv` → **ogval** — desire, wanting
- `S1-gv` → **gvala** — wanting, desiring
- `S2-gv` → **egval** — wishing, hoping
- `S3-gv` → **ugval** — aspiration
- `S1-gv-DYN-ASR` → **gvul** — wants

## use

`-ksf-` use.

- `S0-ksf` → **oksfal** — use, utilization, expenditure
- `S1-ksf` → **ksfala** — using, utilizing
- `S2-ksf` → **eksfal** — serving as, functioning as
- `S3-ksf` → **uksfal** — consumption, using up
- `S1-ksf-DYN-ASR` → **ksful** — uses

S2 is worth noticing. *This serves as a door* and *I use the door* are
the same root in Ithkuil, seen from the thing and from the user.

## find, meet

`-fh-` is finding, and every stem of it is by chance.

- `S0-fh` → **ofhal** — finding, encountering, meeting
- `S1-fh` → **afhal** — finding, discovering by chance
- `S2-fh` → **efhal** — encountering, coming across by chance
- `S3-fh` → **ufhal** — meeting by chance
- `S1-fh-DYN-ASR` → **afhulá** — finds

Deliberate finding is not this root. English *find* covers both the
accident and the result of searching, and only the accident is here.

## help

`-nn-` help.

- `S0-nn` → **onnal** — help, assistance, support
- `S1-nn` → **nnala** — aiding, helping
- `S2-nn` → **ennal** — assisting, lending a hand
- `S3-nn` → **unnal** — supporting, advocating
- `S1-nn-DYN-ASR` → **nnul** — helps

## government

`-ḑf-` government.

- `S0-ḑf` → **oḑfal** — government
- `S1-ḑf` → **aḑfal** — governing, governance
- `S2-ḑf` → **eḑfal** — being political
- `S3-ḑf` → **uḑfal** — a group of authorities recognized as a government
- `S1-ḑf-DYN-ASR` → **aḑfulá** — governs

The English noun is S3; the activity is S1. Forms of government are
separate roots: `-ḑt-` for representative ones, `-llk-` for monarchal,
totalitarian, and oligarchical.

## group

`-d-` is the act of grouping, not the group.

- `S0-d` → **odal** — a group
- `S1-d` → **dala** — assembling, gathering into a group
- `S2-d` → **edal** — congregating, meeting together
- `S3-d` → **udal** — convening for a conference or congress
- `S1-d-DYN-ASR` → **dul** — gathers

For "a group of X" as a plurality of some particular thing, this root is
usually the wrong tool. That is Configuration and Perspective on X
itself, the same machinery as *people* under *person* above.

## number

`-nth-` number.

- `S0-nth` → **onthal** — number, integer, rational number
- `S1-nth` → **anthal** — a number, expressing numerically
- `S2-nth` → **enthal** — an integer
- `S3-nth` → **unthal** — a rational number

Real, irrational, and imaginary numbers are `-rnt-`. English *number*
meaning "a quantity of" is not this root; that is the PTW affix.

## part

`-thw-` component.

- `S0-thw` → **othwal** — component, part, ingredient
- `S1-thw` → **thwala** — a component, part, piece
- `S2-thw` → **ethwal** — an ingredient
- `S3-thw` → **uthwal** — an instruction

`-ţf-` is the positionally-defined sense: `S1-ţf` a part or section of
something, `S3-ţf` a separable part.

## family, parent, child

Two roots. `-mp-` is the nuclear family member.

- `S0-mp` → **ompal** — a nuclear family member
- `S1-mp` → **ampal** — a parent
- `S2-mp` → **empal** — a child, in the sense of offspring
- `S3-mp` → **umpal** — a grandparent

*Child* here is the kin relation. *Child* meaning a young human is
`S2-l` → **elal**, a different root, and English uses one word for both.

`-bč-` is the wider kin relation.

- `S0-bč` → **občal** — kin, extended family relation
- `S1-bč` → **abčal** — a family relation
- `S2-bč` → **ebčal** — a genetic or marital relation
- `S3-bč` → **ubčal** — an adoptive relation

## business

`-dň-` commercial enterprise.

- `S0-dň` → **odňal** — a commercial enterprise, business, industry
- `S1-dň` → **adňal** — conducting business as a commercial enterprise
- `S2-dň` → **edňal** — making a product available for purchase
- `S3-dň` → **udňal** — an industrial enterprise

---

## big, large

English *big* is not one concept. Before anything else, decide which of
these is meant, because they are built out of entirely different
material and only the first is an affix for size.

### 1. Physically large — the common case

**SIZ** `-x-` at degree 7. There is no separate word: the affix goes
inside the word for the thing that is big.

- `S3-rm-SIZ/7` → **wurmox** — a big house
- `S1-l-SIZ/7` → **lalox** — a big adult
- `S2-l-SIZ/7` → **welox** — a big child
- `S1-lḑ-SIZ/7` → **walḑox** — a big tree
- `S1-jl-SIZ/7` → **jlalox** — a big mountain
- `S2-lz-SIZ/7` → **welzox** — a big river
- `S1-zv-SIZ/7` → **zvalox** — a big dog
- `S1-žx-SIZ/7` → **wažxox** — a big fire
- `S1-lks-SIZ/7` → **walksox** — a big meal
- `S1-d-SIZ/7` → **dalox** — a big gathering
- `S1-bč-SIZ/7` → **wabčox** — a big family
- `S1-nth-SIZ/7` → **wanthox** — a big number
- `S1-thw-SIZ/7` → **thwalox** — a big part
- `S1-ţr-G-SIZ/7` → **ţrarox** — a big amount of water

The scale runs 1 to 9 and is anchored in the middle at *the right size*,
so degree 7 is big for that kind of thing: a big mouse and a big house
are both degree 7 and are not the same number of metres. The other
degrees are in the affix table under SIZ; degree 9 is worth knowing
because it means *too* big rather than very big.

### 2. Important, significant

Not SIZ. *A big decision*, *a big problem*, *big news* say nothing about
extent, and putting SIZ on them says the thing is physically large.
`S1-ňf-SIZ/7` → **waňfox** is a problem that takes up space.

Importance is the root `-šh-`.

- `S0-šh` → **ošhal** — importance, significance
- `S1-šh` → **ašhal** — importance, significance
- `S2-šh` → **ešhal** — prevalence
- `S3-šh` → **ušhal** — salience, prominence

There is no importance affix in the affix inventory, so an important
problem is not one word. It is this root and that root in a
concatenation chain, and the chain is left unwritten here until the
compose and parse sides agree on one (see the note at the end).

### 3. Great in degree or extent

*A big drinker*, *big trouble*, *a big spender*. English is describing
how much, not how large, and that is **EXN** `-g-`, degree or extent, at
degree 7.

- `S3-tx-EXN/7` → **wutxog** — a big drinker, one who drinks a great deal
- `S2-tx-EXN/7` → **wetxog** — a big eater
- `S1-šč-EXN/7` → **ščalog** — a big spender
- `S1-rňf-EXN/7` → **warňfog** — big trouble

### 4. Senior, elder

*Big brother*, *big sister* in the family sense are about age, not size.
That is the stem of the kinship root, or the age stems of `-l-` under
*person*. Putting SIZ/7 on a sibling says they are physically large,
which is a different and often wrong claim.

### 5. Idioms, where the whole phrase is one concept

These are not *big* plus a noun at all. The English phrase is a single
lexical item and it translates as whatever it means, not as its parts.

- `S1-ẓdr` → **ẓdrala** — the big house, jail, incarceration
- `S2-lxm` → **elxmal** — Mr. Big, the boss, the one in charge
- `S1-vsk` → **avskal** — Big Brother, surveillance
- `S3-mfl` → **umflal** — the big day, a wedding
- `S3-šh` → **ušhal** — a big shot, a big name, a prominent one

Note that *big house* has both readings and they share nothing.
**wurmox** is a large dwelling. **ẓdrala** is a prison. An English
speaker who reaches for the first when they mean the second has not made
a small error of degree; they have said an unrelated thing.

## small, little

The same affix, low degrees. Everything above applies unchanged.

- `S3-rm-SIZ/3` → **wurmex** — a small house
- `S1-l-SIZ/3` → **lalex** — a small adult
- `S2-l-SIZ/3` → **welex** — a small child
- `S1-lḑ-SIZ/3` → **walḑex** — a small tree
- `S1-zv-SIZ/3` → **zvalex** — a small dog

English *little* often carries affection rather than size, as in *the
little ones*. That is not SIZ, and putting degree 3 on a child says the
child is undersized.

### A note on the concatenation gap

*An important problem* should be a Type-1 or Type-2 concatenation chain
of `-šh-` and `-ňf-`. `ithkuil compose "T2-šh"` produces **hrašha**, but
`ithkuil parse "hrašha"` cannot read it back, so no chain is written into
this file yet. Every form here round-trips, and that one does not.
