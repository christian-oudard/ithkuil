package parse

import "github.com/christian-oudard/ithkuil/fault"

// The adjunct decoders each answer one question — is this word a
// carrier, a modular, an affixual adjunct? — and a "no" comes in two
// kinds that are not worth the same to a reader.
//
// A shape "no" means the word was never a candidate: it has the wrong
// number of conjuncts, or a consonant where the class wants a vowel.
// Every class says this about almost every word, so on its own it is
// noise.
//
// A value "no" means the shape fit and a table did not list what was
// written in it. That is a word someone was plainly trying to write,
// with one thing wrong, and it is the complaint worth surfacing when
// several classes have all refused a word.
//
// The two are separated here rather than at the caller because only
// the decoder knows which it just found.

// shape reports that the word does not have this class's shape.
func shape(word, slot, found, admits string) error {
	return fault.One(word, fault.Fault{
		Stage: fault.Shape, Code: slot, Found: found, Fix: admits,
	})
}

// value reports that the shape fit but a slot holds a form its table
// does not list.
func value(word, slot, found, admits string) error {
	return fault.One(word, fault.Fault{
		Stage: fault.Value, Code: slot, Found: found, Fix: admits,
	})
}
