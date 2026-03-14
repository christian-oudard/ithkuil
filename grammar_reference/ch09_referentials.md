# Chapter 9: Referentials

Referentials function like personal pronouns in other languages, but with richer morphology. Each referential encodes a **referent** (who/what is being referred to), an **effect** (the speaker's attitude toward that party's involvement), and a **case** (the grammatical role of the referent).

## 9.1 Referent Categories

There are 11 referent categories:

| Abbrev. | Category | Meaning |
|---------|----------|---------|
| **1m** | Monadic speaker | "I" |
| **2m** | Monadic addressee | "you (sg.)" |
| **2p** | Polyadic addressee | "you (pl.)" |
| **ma** | Monadic animate 3rd party | "he" / "she" / "they (sg.)" |
| **pa** | Polyadic animate 3rd party | "they (pl.)" |
| **mi** | Monadic inanimate 3rd party | "it" |
| **pi** | Polyadic inanimate 3rd party | "these things" / "those things" |
| **Mx** | Mixed animate/inanimate 3rd party | mixed animate+inanimate, e.g. "s/he+it", "they+those" |
| **Rdp** | Reduplicative (resumptive) | refers back to the previously named party (often used with SWR affix) |
| **Obv** | Obviative | 3rd party other than one previously referenced |
| **PVS** | Provisional | "whatever" (vague, unspecified, potential, uncertain, or unknown party) |

## 9.2 Effect

Each referent is marked for one of three **effects**, indicating the speaker's assessment of whether the referent's participation is neutral, beneficial, or detrimental to the referent itself. (See Sec. 5.4 of the grammar for a full explanation of Effect.)

| Effect | Abbrev. | Meaning |
|--------|---------|---------|
| Neutral | NEU | No particular benefit or detriment to the referent |
| Beneficial | BEN | The referent benefits from the situation |
| Detrimental | DET | The referent is harmed or disadvantaged by the situation |

## 9.3 C1 Consonant Forms

The C1 consonant encodes both the referent category and the effect. The full table of C1 values:

| Referent | Category | NEU | BEN | DET | Gloss |
|----------|----------|-----|-----|-----|-------|
| **1m** | monadic speaker | **l** | **r** | **ř** | "I" |
| **2m** | monadic addressee | **s** | **š** | **ž** | "you (sg.)" |
| **2p** | polyadic addressee | **n** | **t** | **d** | "you (pl.)" |
| **ma** | monadic animate 3rd party | **m** | **p** | **b** | "he/she" |
| **pa** | polyadic animate 3rd party | **ň** | **k** | **g** | "they (pl.)" |
| **mi** | monadic inanimate 3rd party | **z** | **ţ** | **ḑ** | "it" |
| **pi** | polyadic inanimate 3rd party | **ẓ** | **f** | **v** | "those things" |
| **Mx** | mixed animate/inanimate | **c** | **č** | **j** | "s/he+it" etc. |
| **Rdp** | reduplicative (resumptive) | **th** | **ph** | **kh** | "aforementioned" |
| **Obv** | obviative | **ll** / lç\* | **rr** / rç\* | **řř** / řç\* | "the other one" |
| **PVS** | provisional | **mm** / mç\* | **nn** / nç\* | **ňň** / ňç\* | "whatever" |

\* The alternate forms (lç, rç, řç, mç, nç, ňç) are used in Referential Affixes (Sec. 4.6.5) to avoid ambiguity with geminated Ca forms.

Note: The **pi/NEU** referent also has an alternate C1 form **ļ** (alongside **ẓ**).

### Biconsonantal Forms

The following C1 values are biconsonantal (two characters) and must be checked before single-consonant forms during parsing:

- Rdp: **th**, **ph**, **kh**
- Obv: **ll**, **rr**, **řř**
- PVS: **mm**, **nn**, **ňň**

## 9.4 Single-Referent Referential Structure

The simplest referential consists of a C1 consonant (the referent) followed by a Vc vowel (the case):

| Slot 1 | Slot 2 |
|--------|--------|
| (ë-)C1((ë-)C1((ë-)C1)) | Vc |
| Referent(s) A | Case of Referent A (same 68 case affixes as formative Slot IX) |

- Multiple C1 consonants may be combined in Slot 1 (e.g., **smlo** = "you(sg.) and s/he and I"-ERG) as long as the combination is phonotactically permissible.
- An epenthetic **ë** vowel appears before or within C1 combinations if necessary for phonotactic reasons (e.g., **zëmse** = "it and s/he and you(sg.)-ABS").

### Examples

- **la** = 1m/NEU-THM = "I" (thematic case)
- **sa** = 2m/NEU-THM = "you (sg.)" (thematic case)
- **mei** = ma/NEU-INS = "by means of him/her"
- **zëmse** = mi/NEU + ma/NEU + 2m/NEU - ABS = "it and s/he and you (sg.)"

## 9.5 Dual-Referent Referential Structure

When two referents each need a different case, use the dual-referent form:

| Slot 1 | Slot 2 | Slot 3 | Slot 4 | Slot 5 |
|--------|--------|--------|--------|--------|
| (ë-)C1((ë-)C1((ë-)C1)) | Vc1 | (w/y + Vc2) | (C2(ë-)) | Stress |
| Referent A | Case of Referent A | Case of Referent B, or 2nd (stacked) case on Referent A | Referent B | See below |

**Stress:**
- Monosyllabic or penultimate stress = default (NRM Essence)
- Ultimate stress = RPV Essence

**Notes:**
- The w/y in Slot 3 serves as a glide linking the two case vowels.
- If Slot 4 is left empty but Slot 3 is filled, the second case is **stacked** onto Referent A (i.e., a single referent bearing two cases).
- For personal referents other than 1m, two separate instances of the same referent can be shown by placing one C1 in Slot 1 and another in Slot 4 (e.g., **püwüp** [ma/BEN/DAT - ma/BEN/DAT] = "to him/her and to (a different) him/her", **zäwiez** [mi/INS - mi/TRA] = "with it and for (a different) it").

### Tell-tale Sign

A referential is distinguished from a formative by its (ë-)C(C)-V or (ë-)C(C)-V-w/y-V-C structure. Formative Slot II has no Vv value of -ë-, nor does Slot IV Vr contain -w- or -y-.

### Examples

- **ëztewim** = mi/NEU + 2p/NEU - ma/NEU
- **smoyút** = 2m/NEU + ma/NEU - ultimate stress (RPV)
- **triwejvë** = dual referent with case stacking

## 9.6 Combination Referential (with Case-Stacking)

A third structure allows attaching VxCs affix information or a second case to one of the referential forms:

| Slot 1 | Slot 2 | Slot 3 | Slot 4 | Slot 5 | Slot 6 |
|--------|--------|--------|--------|--------|--------|
| (ë-)C1(C2(C3)) | Vc | x / xt / xp / xx | VxCs... | (Vc2 or epenthetic -a) | Stress |
| Combination Referent | Case of Referent | Specification | VxCs suffix(es) | 2nd (stacked) case | See below |

**Specification markers in Slot 3:**
- **x** = BSC (Basic)
- **xt** = CTE (Contential)
- **xp** = CSV (Constitutive)
- **xx** = OBJ (Objective)

**Stress:**
- Penultimate stress = default
- Ultimate stress = RPV Essence

The tell-tale signs of a Combination Referential are the Slot 3 consonant forms containing -x-, -xt-, -xp-, or -xx- in what would otherwise look like the Ca slot of a formative (these four conjuncts are not valid Ca forms).

### Examples

- **slex** = combination referential with BSC specification
- **ëtkexpa** = combination referential with CTE specification + suffix

## 9.7 Agglomerative, Nomic, and Abstract Referentials

To show agglomerative, nomic, or abstract categories as a referential, add the following affixes immediately preceding or following a C1 form (as phonotactically permissible):

| Category | Prefix form | Suffix form |
|----------|-------------|-------------|
| Agglomerative | **ļ-** | **-tļ-** |
| Nomic | **ç-** | **-x-** |
| Abstract | **w-** | **-y** |

### Impersonal Referentials

To express impersonal categories such as "one", "someone", "something", "a thing", "things", etc., add the **nomic** affix to the **ma** or **mi** referential forms, depending on the intended meaning.

Applying the **abstract** affix to a tangible referential form gives a word meaning "all that X is" or "everything about X" -- as in "all about me", "everything having to do with you", or "everything about it". When applied to the **Mx** referential, the abstract affix gives the meaning "everything and everyone" or "all that there is."

## 9.8 The PVS Provisional Referential

The PVS Provisional Referential refers to a vague, unspecified, potential, uncertain, or unknown party/entity which can be marked for case. Since the identity of the entity is provisional/potential/uncertain, the focus is on the case-relationship being expressed, so the meaning of the case often determines the proper English translation. For example:

- PVS-LOC *where?* ("whatever location")
- PVS-PUR *why?* ("whatever purpose")
- PVS-PRP *whose?* ("whatever owner")

In phrases/sentences using IRG Illocution, the PVS Referential corresponds most closely to English WH-interrogative pronouns.

See Sec. 10.6 of the grammar for further details on WH-interrogatives.

## 9.9 Suppletive Adjuncts as Referential C1 Forms

In addition to the 11 referent categories, the C1 slot of a Single-, Dual-, or Combination Referential may also take any of the four **suppletive adjunct** consonant forms:

| Adjunct | Cp form | Name | Meaning |
|---------|---------|------|---------|
| **CAR** | **hl** | Carrier | A "shortcut" carrier stem providing case information only; used when the identity/nature of the following foreign word(s) or proper name is already known to the addressee |
| **QUO** | **hm** | Quotative | Combines the Carrier Adjunct with the DSV (Discursive) Register to allow direct quotes within sentences (e.g., *He told me "get out of the house!"*) |
| **NAM** | **hn** | Naming | Indicates the following word is a name being referred to *as such* (the name itself, not the entity bearing it). Used before a name in *"He said 'Emily'"* vs. QUO for *"He said 'tell Emily'"* |
| **PHR** | **hň** | Phrasal | A specialized carrier form that applies meta-level grammatical information to an entire subsequent phrase as a whole, creating a conventionalized lexicalized gestalt (equivalent to English phrasal gestalts like "know-how-to-get-things-done") |

### Epenthetic Vowel Rules

When using a Cp suppletive adjunct form in Slot 1 of a referential, a word-initial epenthetic vowel is required:

- In a **Combination Referential** (Sec. 9.6): prefix the Cp value with **a-** (to avoid being mistaken for a concatenated formative)
- In a **Single- or Dual-Referential** (Sec. 9.4/9.5): prefix the Cp value with the diphthong **üo-**

### Examples

| Adjunct | Example forms |
|---------|-------------|
| CAR (**hl**) | *hla, hlei, hloa, hle'e, hla'u, hli'a* |
| QUO (**hm**) | *hma, hmei, hmoa, hme'e, hma'u, hmi'a* |
| NAM (**hn**) | *hna, hnei, hnoa, hne'e, hna'u, hni'a* |
| PHR (**hň**) | *hňa, hňei, hňo, hňe'e, hňa'u, hňi'a* |

Use of these suppletive adjuncts implies **CCN** (Concatenated) Case-scope. If the case-framed word/phrase/name has non-default Case-scope, use either a full carrier stem or a preceding adjunct to show the case-scope.

The last word of the word, phrase, or name identified by the adjunct can be indicated by the adjunct form **hü** if necessary, or by pronouncing that word/phrase/name with low tone.

Note: Since Sec. 1.5 external juncture rules do not apply to foreign names/words, insert a pause after uttering the name/words prior to the CAR adjunct *hü* or the SPF final adjunct *hiu*, or pronounce the last word of the proper name/phrase with low tone.
