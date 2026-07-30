// Package roman is the romanization arm: grammar to written Ithkuil
// and back.
//
// Both directions live here because they encode one thing — which
// letters spell which grammar — and splitting them by direction is
// what let them drift apart. A renderer and a parser that disagree
// produce a word that reads back as something else, and neither one
// can notice on its own.
//
// The reading direction takes a Parse prefix, the writing direction
// keeps the noun:
//
//	ParseFormative / Formative
//	ParseReferential / Referential
//	ParseWord / Word
//	ParseText / Text
//
// which is the shape package gloss has for its own arm, so the two
// peripheral formats read the same way.
//
// Tokenize is the per-word report: it pairs each romanization with the
// word it produced, or with the reason there is none. ParseText is the
// all-or-nothing form for callers that do not need to say which word
// failed.
//
// Stressless writes a span with its stress carried by §4.8 parsing
// adjuncts instead of the acute and circumflex, for the channel §2.3
// ¶5 describes where pitch accent cannot mark word boundaries.
package roman
