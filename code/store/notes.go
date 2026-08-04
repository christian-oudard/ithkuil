package store

// Note is one grammar value's authored explanation, and Topic is one
// belonging to no single value. They are returned as plain structs so
// that a caller can hand them to whatever wants them without this
// package knowing what that is.
type Note struct {
	Abbrev      string
	Explanation string
	Guidance    string
}

// Notes returns every grammar value that carries authored text, and
// every topic. 160 of the 294 values have one; a value with nothing
// surprising about it has none, which is not an oversight.
func (s *Store) Notes() ([]Note, []Topic, error) {
	all, err := s.GrammarAll()
	if err != nil {
		return nil, nil, err
	}
	var notes []Note
	for _, e := range all {
		if e.Explanation == "" && e.Guidance == "" {
			continue
		}
		notes = append(notes, Note{
			Abbrev: e.Abbrev, Explanation: e.Explanation, Guidance: e.Guidance,
		})
	}
	topics, err := s.Topics()
	if err != nil {
		return nil, nil, err
	}
	return notes, topics, nil
}
