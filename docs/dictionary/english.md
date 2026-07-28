# English to Ithkuil

The mapping from English into Ithkuil, written as the definition
language underneath English. Authored by hand rather than generated.
Other languages get their own file beside this one.

## What belongs here, and what does not

Nothing derivable from the lexicon is written down. Package `dictionary`
already reads the English out of the root glosses at runtime, so *trout*
and *crisis* need no entry here; `ithkuil define trout` finds them
without one. Writing them down a second time would only let the two
copies drift.

What belongs here is everything the lexicon cannot answer:

- English words that no root names, where the Ithkuil form has to be
  composed from a root plus a specification and affixes.
- Function words, which are not lexical at all in Ithkuil. *Of* is a
  case, *not* is an affix, *the* is nothing.
- Corrections, where the lexicon's own English is misleading enough that
  the derived answer should be overridden.

## Entry format

Each entry is a headword, one or more Ithkuil expressions in the
canonical gloss syntax, and the reasoning. The gloss expression is the
authoritative half: it is what `ithkuil compose` reads, so every entry
can be checked by composing it into a real word. The prose is what makes
this a teaching document rather than a table.

The gloss expression, not the English word, is the identity of an entry.
When the other languages arrive they attach to the same expression, and
English becomes one label among many rather than the key everything
hangs off.
