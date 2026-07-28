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

English comes first, since that is the side you arrive from. Each line
reads: the English, the Ithkuil word, and the gloss expression that
builds it.

    - a child → **elal** `S2-l`

The gloss expression is the authoritative part. It is what
`ithkuil compose` reads, so every line here can be checked:

    $ ithkuil compose "S2-l"
    elal   S2/PRC--l-'human child'

Every form below was composed that way, and every phrase was read back
with `ithkuil parse` to confirm it parses to what it claims. The gloss
expression, not the English, is the identity of an entry: when other
languages arrive they attach to the same expression, and English becomes
one label among several rather than the key everything hangs off.

Stems are written S1, S2, S3, and S0 for the general reading that covers
all three. `DYN` makes the word verb-like, an act rather than an entity.
`ASR` makes it a verb proper, an assertion. `G` is Agglomerative
perspective, which is where English mass nouns live.

---

## person, people

`-l-` is the human being. The stem carries the age, which English splits
across separate words.

- a human being, age not at issue → **olal** `S0-l`
- an adult → **lala** `S1-l`
- a child → **elal** `S2-l`
- an adolescent → **ulal** `S3-l`

English *people* is not a word here, and it is worth being careful about
which sense is meant. For "one or more humans, the number being beside
the point", use Agglomerative perspective:

- adults, any number of them → **lara** `S1-l-G`

For a true plural, "two or more", §3.6 is explicit that Agglomerative
does not do it and the **XX2** affix at degree 6 does:

- two or more adults → **lalöks** `S1-l-XX2/6`

*Man* and *woman* are not stems of this root. Sex is not part of the
lexical identity of a human in Ithkuil, and asking for *man* the way one
asks for *child* is the wrong shape of question.

## water

`-ţr-` water. The stems separate the sources.

- water → **ţrala** `S1-ţr`
- natural fresh water → **eţral** `S2-ţr`
- artificial fresh water → **uţral** `S3-ţr`

The important thing for an English speaker is that the plain form is a
count noun. §3.6 uses this exact word as its example: Monadic **ţrala**
is *a drop of water*, a single unit of it, and the mass noun English
reaches for by default is the Agglomerative.

- water, some water, an amount of water → **ţrara** `S1-ţr-G`

Wanting *water* and writing **ţrala** gets *a drop*. This trap is
general: rice, hair, sand, money all behave the same way.

## eat, drink

`-tx-` is nutritional consumption, one root where English has two verbs.

- consuming → **txala** `S1-tx`
- eating food → **etxal** `S2-tx`
- drinking liquid → **utxal** `S3-tx`

As verbs:

- eats, is eating → **etxulá** `S2-tx-DYN-ASR`
- drinks, is drinking → **utxulá** `S3-tx-DYN-ASR`

In use, with the drinker as Ergative and what is drunk as Absolutive:

    utxulá welo ţrare
    The child drinks water.

## give, receive

`-n-` is the transfer of possession itself. English needs two verbs
because it names the transfer from each end; Ithkuil names the transfer
and lets the stem pick the end.

- a transfer of possession → **nala** `S1-n`
- giving → **enal** `S2-n`
- receiving → **unal** `S3-n`

As verbs:

- gives → **enulá** `S2-n-DYN-ASR`
- receives → **unulá** `S3-n-DYN-ASR`

This is the shape to expect for many English verb pairs. *Buy* and
*sell*, *teach* and *learn*, *lend* and *borrow* are each one event seen
from two ends, and the language treats them that way.

## see

`-ẓ-` vision.

- sight, vision → **oẓal** `S0-ẓ`
- sight, the sense → **ẓala** `S1-ẓ`
- an eye, the organ → **eẓal** `S2-ẓ`
- one's visual faculty → **uẓal** `S3-ẓ`
- sees → **ẓul** `S1-ẓ-DYN-ASR`

Note that the organ is a stem of the same root as the sense. Ithkuil
groups faculty, organ, and capacity under one lexical identity where
English has *sight* and *eye* as unrelated words.

## say, speak

`-m-` is linguistic utterance for communication.

- an utterance → **mala** `S1-m`
- speaks, says something → **mul** `S1-m-DYN-ASR`

## know

`-ţt-` knowing, and the stems sort it by how the knowledge was got, a
distinction English does not mark at all.

- fact, knowing → **oţtal** `S0-ţt`
- knowing an ontological fact, that something is so → **ţtala** `S1-ţt`
- knowing an epistemological fact, knowing it as knowledge → **eţtal** `S2-ţt`
- knowing from experience → **uţtal** `S3-ţt`
- knows → **ţtul** `S1-ţt-DYN-ASR`

## think

`-sl-` thought.

- thought, idea, concept → **oslal** `S0-sl`
- thinking, cogitation → **slala** `S1-sl`
- consideration, mulling → **eslal** `S2-sl`
- reasoning → **uslal** `S3-sl`
- thinks → **slul** `S1-sl-DYN-ASR`

English *think* also carries "believe" and "have an opinion", which is a
different notion and does not belong to this root.

## make

`-ţk-` making, and the stems distinguish how the thing is made.

- make, construct, form → **oţkal** `S0-ţk`
- making, construction → **ţkala** `S1-ţk`
- forming by combining ingredients → **eţkal** `S2-ţk`
- organizing disparate parts into a whole → **uţkal** `S3-ţk`
- makes → **ţkul** `S1-ţk-DYN-ASR`

## work

`-rtm-` work, labor.

- work, labor, job → **ortmal** `S0-rtm`
- work, labor → **artmal** `S1-rtm`
- works → **artmulá** `S1-rtm-DYN-ASR`

## time

English *time* is several unrelated notions, and they take different
roots.

Elapsed time, duration:

- a temporal interval → **ogẓal** `S0-gẓ`
- a degree of elapsed time → **gẓala** `S1-gẓ`

Time of day:

- daytime → **alnal** `S1-ln`
- nighttime → **elnal** `S2-ln`
- o'clock time, the time on a clock → **ulnal** `S3-ln`

Asking for "the word for time" has no answer. Which of these is meant
has to be settled before the sentence can be built, and that is the
normal condition of translating into Ithkuil.

## day, week, month, year

Two roots carry the calendar between them, split at the year.

`-ř-` is the conventionalized time period, the ones a body or a moon
gives you.

- a day, a 24-hour period → **řala** `S1-ř`
- a week → **eřal** `S2-ř`
- a month, a lunar period → **uřal** `S3-ř`

`-rn-` is the longer periods.

- a calendar year → **arnal** `S1-rn`
- a decade → **ernal** `S2-rn`
- a lifetime, an expected span → **urnal** `S3-rn`

*Day* meaning the 24-hour unit and *day* meaning the daylight hours are
different roots, and English runs them together. Daytime is `-ln-`,
under *time* above.

`-rḑ-` is the same series again but designated: `S1-rḑ` → **arḑal**, a
particular day on a calendar rather than a day as a span.

## night

- nighttime → **elnal** `S2-ln`

## place

`-ţkl-` place.

- setting, place, site, venue → **oţklal** `S0-ţkl`
- a setting → **aţklal** `S1-ţkl`
- a place, a site → **eţklal** `S2-ţkl`

## house, home

Two roots, and English blurs them.

`-rm-` is the physical dwelling.

- a dwelling, where an entity lives → **armal** `S1-rm`
- a domicile, an artificial dwelling for protection → **ermal** `S2-rm`
- a house, a constructed residence with conveniences → **urmal** `S3-rm`

`-nkr-` is one's surroundings, and *home* in the sense of where one
belongs.

- home → **ankral** `S1-nkr`
- environmental circumstances → **enkral** `S2-nkr`
- the world → **unkral** `S3-nkr`

## world

`S3-nkr` → **unkral**. The world is the third stem of the surroundings
root, the outermost ring of *home*. There is no separate word for it.

## life

`-šw-` the living.

- a lifeform → **ošwal** `S0-šw`
- a living being, a lifeform → **šwala** `S1-šw`
- vitality, activity consistent with being alive → **ešwal** `S2-šw`
- being brought to life, biological reproduction → **ušwal** `S3-šw`

English *life* also means "a lifetime" (`S3-rn` → **urnal**) and "the
way one lives", which is a different root again. The three are not
interchangeable.

## money

`-šč-` money and financial transaction.

- money, financial transaction, banking → **oščal** `S0-šč`
- a medium of exchange → **ščala** `S1-šč`
- money, currency → **eščal** `S2-šč`
- a quasi-contractual document → **uščal** `S3-šč`

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

- a good adult, one who is effective at it → **lalöth** `S1-l-QUA/6`
- a bad, poor, inadequate one → **laleth** `S1-l-QUA/3`

Degree 5 is *not bad, adequate*; degree 8 is *excellent*; degree 9 is
*too good*. Choosing among nine degrees is the normal cost of saying
*good* in Ithkuil.

**NEW** `-sp-`, newness and revision, and the degrees are distinctions
English does not draw. Degree 1 is never before seen at all; degree 2 is
new only within the present context, the sense in *a new student*.

- an adult never before seen → **lalasp** `S1-l-NEW/1`
- an adult new to this setting → **laläsp** `S1-l-NEW/2`

**MDN** `-nţ-`, degree of age or modernity, covers *old* in the sense of
era: degree 3 ancient, 4 old as in first, 5 old as in former, 6 modern.

- an ancient adult → **lalenţ** `S1-l-MDN/3`

*Old* meaning aged is not this affix. A person's age is carried by the
stems of `-l-` under *person* above, and old age proper is `-dç-`.

## man, woman, boy, girl

Sex is the **SEX** affix `-š-`, not a stem and not a root. Degree 1 is
female, degree 3 male; 5 is intersex, 6 unknown, 9 sex-neutral.

- a man → **laleš** `S1-l-SEX/3`
- a woman → **lalaš** `S1-l-SEX/1`
- a boy → **weleš** `S2-l-SEX/3`
- a girl → **welaš** `S2-l-SEX/1`

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

- ambulation → **ogal** `S0-g`
- natural ambulation, walking or crawling → **gala** `S1-g`
- rapid ambulation, running or galloping → **egal** `S2-g`
- unnatural ambulation, limping or staggering → **ugal** `S3-g`
- walks → **gul** `S1-g-DYN-ASR`

Whether it is walking or crawling depends on the creature, not on the
word. The stem names the relation of the gait to the body that has it.

## want

`-gv-` desire.

- desire, wanting → **ogval** `S0-gv`
- wanting, desiring → **gvala** `S1-gv`
- wishing, hoping → **egval** `S2-gv`
- aspiration → **ugval** `S3-gv`
- wants → **gvul** `S1-gv-DYN-ASR`

## use

`-ksf-` use.

- use, utilization, expenditure → **oksfal** `S0-ksf`
- using, utilizing → **ksfala** `S1-ksf`
- serving as, functioning as → **eksfal** `S2-ksf`
- consumption, using up → **uksfal** `S3-ksf`
- uses → **ksful** `S1-ksf-DYN-ASR`

S2 is worth noticing. *This serves as a door* and *I use the door* are
the same root in Ithkuil, seen from the thing and from the user.

## find, meet

`-fh-` is finding, and every stem of it is by chance.

- finding, encountering, meeting → **ofhal** `S0-fh`
- finding, discovering by chance → **afhal** `S1-fh`
- encountering, coming across by chance → **efhal** `S2-fh`
- meeting by chance → **ufhal** `S3-fh`
- finds → **afhulá** `S1-fh-DYN-ASR`

Deliberate finding is not this root. English *find* covers both the
accident and the result of searching, and only the accident is here.

## help

`-nn-` help.

- help, assistance, support → **onnal** `S0-nn`
- aiding, helping → **nnala** `S1-nn`
- assisting, lending a hand → **ennal** `S2-nn`
- supporting, advocating → **unnal** `S3-nn`
- helps → **nnul** `S1-nn-DYN-ASR`

## government

`-ḑf-` government.

- government → **oḑfal** `S0-ḑf`
- governing, governance → **aḑfal** `S1-ḑf`
- being political → **eḑfal** `S2-ḑf`
- a group of authorities recognized as a government → **uḑfal** `S3-ḑf`
- governs → **aḑfulá** `S1-ḑf-DYN-ASR`

The English noun is S3; the activity is S1. Forms of government are
separate roots: `-ḑt-` for representative ones, `-llk-` for monarchal,
totalitarian, and oligarchical.

## group

`-d-` is the act of grouping, not the group.

- a group → **odal** `S0-d`
- assembling, gathering into a group → **dala** `S1-d`
- congregating, meeting together → **edal** `S2-d`
- convening for a conference or congress → **udal** `S3-d`
- gathers → **dul** `S1-d-DYN-ASR`

For "a group of X" as a plurality of some particular thing, this root is
usually the wrong tool. That is Configuration and Perspective on X
itself, the same machinery as *people* under *person* above.

## number

`-nth-` number.

- number, integer, rational number → **onthal** `S0-nth`
- a number, expressing numerically → **anthal** `S1-nth`
- an integer → **enthal** `S2-nth`
- a rational number → **unthal** `S3-nth`

Real, irrational, and imaginary numbers are `-rnt-`. English *number*
meaning "a quantity of" is not this root; that is the PTW affix.

## part

`-thw-` component.

- component, part, ingredient → **othwal** `S0-thw`
- a component, part, piece → **thwala** `S1-thw`
- an ingredient → **ethwal** `S2-thw`
- an instruction → **uthwal** `S3-thw`

`-ţf-` is the positionally-defined sense: `S1-ţf` a part or section of
something, `S3-ţf` a separable part.

## family, parent, child

Two roots. `-mp-` is the nuclear family member.

- a nuclear family member → **ompal** `S0-mp`
- a parent → **ampal** `S1-mp`
- a child, in the sense of offspring → **empal** `S2-mp`
- a grandparent → **umpal** `S3-mp`

*Child* here is the kin relation. *Child* meaning a young human is
`S2-l` → **elal**, a different root, and English uses one word for both.

`-bč-` is the wider kin relation.

- kin, extended family relation → **občal** `S0-bč`
- a family relation → **abčal** `S1-bč`
- a genetic or marital relation → **ebčal** `S2-bč`
- an adoptive relation → **ubčal** `S3-bč`

## business

`-dň-` commercial enterprise.

- a commercial enterprise, business, industry → **odňal** `S0-dň`
- conducting business as a commercial enterprise → **adňal** `S1-dň`
- making a product available for purchase → **edňal** `S2-dň`
- an industrial enterprise → **udňal** `S3-dň`

---

## big, large

English *big* is not one concept. Before anything else, decide which of
these is meant, because they are built out of entirely different
material and only the first is an affix for size.

### 1. Physically large — the common case

**SIZ** `-x-` at degree 7. There is no separate word: the affix goes
inside the word for the thing that is big.

- a big house → **wurmox** `S3-rm-SIZ/7`
- a big adult → **lalox** `S1-l-SIZ/7`
- a big child → **welox** `S2-l-SIZ/7`
- a big tree → **walḑox** `S1-lḑ-SIZ/7`
- a big mountain → **jlalox** `S1-jl-SIZ/7`
- a big river → **welzox** `S2-lz-SIZ/7`
- a big dog → **zvalox** `S1-zv-SIZ/7`
- a big fire → **wažxox** `S1-žx-SIZ/7`
- a big meal → **walksox** `S1-lks-SIZ/7`
- a big gathering → **dalox** `S1-d-SIZ/7`
- a big family → **wabčox** `S1-bč-SIZ/7`
- a big number → **wanthox** `S1-nth-SIZ/7`
- a big part → **thwalox** `S1-thw-SIZ/7`
- a big amount of water → **ţrarox** `S1-ţr-G-SIZ/7`

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

- importance, significance → **ošhal** `S0-šh`
- importance, significance → **ašhal** `S1-šh`
- prevalence → **ešhal** `S2-šh`
- salience, prominence → **ušhal** `S3-šh`

There is no importance affix in the affix inventory, so an important
problem is not one word. It is this root and that root in a
concatenation chain, and the chain is left unwritten here until the
compose and parse sides agree on one (see the note at the end).

### 3. Great in degree or extent

*A big drinker*, *big trouble*, *a big spender*. English is describing
how much, not how large, and that is **EXN** `-g-`, degree or extent, at
degree 7.

- a big drinker, one who drinks a great deal → **wutxog** `S3-tx-EXN/7`
- a big eater → **wetxog** `S2-tx-EXN/7`
- a big spender → **ščalog** `S1-šč-EXN/7`
- big trouble → **warňfog** `S1-rňf-EXN/7`

### 4. Senior, elder

*Big brother*, *big sister* in the family sense are about age, not size.
That is the stem of the kinship root, or the age stems of `-l-` under
*person*. Putting SIZ/7 on a sibling says they are physically large,
which is a different and often wrong claim.

### 5. Idioms, where the whole phrase is one concept

These are not *big* plus a noun at all. The English phrase is a single
lexical item and it translates as whatever it means, not as its parts.

- the big house, jail, incarceration → **ẓdrala** `S1-ẓdr`
- Mr. Big, the boss, the one in charge → **elxmal** `S2-lxm`
- Big Brother, surveillance → **avskal** `S1-vsk`
- the big day, a wedding → **umflal** `S3-mfl`
- a big shot, a big name, a prominent one → **ušhal** `S3-šh`

Note that *big house* has both readings and they share nothing.
**wurmox** is a large dwelling. **ẓdrala** is a prison. An English
speaker who reaches for the first when they mean the second has not made
a small error of degree; they have said an unrelated thing.

## small, little

The same affix, low degrees. Everything above applies unchanged.

- a small house → **wurmex** `S3-rm-SIZ/3`
- a small adult → **lalex** `S1-l-SIZ/3`
- a small child → **welex** `S2-l-SIZ/3`
- a small tree → **walḑex** `S1-lḑ-SIZ/3`
- a small dog → **zvalex** `S1-zv-SIZ/3`

English *little* often carries affection rather than size, as in *the
little ones*. That is not SIZ, and putting degree 3 on a child says the
child is undersized.

### A note on the concatenation gap

*An important problem* should be a Type-1 or Type-2 concatenation chain
of `-šh-` and `-ňf-`. `ithkuil compose "T2-šh"` produces **hrašha**, but
`ithkuil parse "hrašha"` cannot read it back, so no chain is written into
this file yet. Every form here round-trips, and that one does not.
