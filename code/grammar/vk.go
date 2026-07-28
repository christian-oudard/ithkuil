package grammar

// Vk is the sealed sum type for the verbal-formative ending. One
// variant per illocution: Assertive carries a Validation; the eight
// non-ASR illocutions are leaf variants (Validation does not apply
// per §3.9.3.2). Each variant exposes a Tag() string method for
// gloss/CLI labelling.
type Vk interface {
	vk()
	Tag() string
}

// Assertive — ASR illocution, paired with one of nine Validations.
type Assertive struct{ Validation Validation }

func (Assertive) vk()         {}
func (Assertive) Tag() string { return "ASR" }

type Directive struct{}

func (Directive) vk()         {}
func (Directive) Tag() string { return "DIR" }

type Declarative struct{}

func (Declarative) vk()         {}
func (Declarative) Tag() string { return "DEC" }

type Interrogative struct{}

func (Interrogative) vk()         {}
func (Interrogative) Tag() string { return "IRG" }

type Verificative struct{}

func (Verificative) vk()         {}
func (Verificative) Tag() string { return "VER" }

type Admonitive struct{}

func (Admonitive) vk()         {}
func (Admonitive) Tag() string { return "ADM" }

type Potentiative struct{}

func (Potentiative) vk()         {}
func (Potentiative) Tag() string { return "POT" }

type Hortative struct{}

func (Hortative) vk()         {}
func (Hortative) Tag() string { return "HOR" }

type Conjectural struct{}

func (Conjectural) vk()         {}
func (Conjectural) Tag() string { return "CNJ" }

// AllVk lists one canonical Vk value per illocution. Assertive carries
// OBS Validation; the other eight are leaf values.
var AllVk = []Vk{
	Assertive{Validation: OBS},
	Directive{}, Declarative{}, Interrogative{}, Verificative{},
	Admonitive{}, Potentiative{}, Hortative{}, Conjectural{},
}
