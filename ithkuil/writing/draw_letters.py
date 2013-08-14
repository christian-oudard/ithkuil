from ithkuil.writing.consonant import consonants
from ithkuil.writing.bottom_ending import bottom_endings
from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset, typeset_fixed
from canoepaddle.mode import *

#DEBUG redefinition of parts lists for testing.
import ithkuil.writing.consonant as cons
#consonants = consonants[-6:]
consonants = [cons.CHacek, cons.LCedilla, cons.Q, cons.G, cons.D, cons.T, cons.K, cons.RHacek, cons.L, cons.J]
import ithkuil.writing.side_ending as se
#side_endings = [se.Normal, se.SideAll]
side_endings = [se.Normal]
import ithkuil.writing.bottom_ending as be
bottom_endings = bottom_endings[-3:]
#bottom_endings = [be.Normal]
#bottom_endings = [be.Normal, be.BottomAll]

if __name__ == '__main__':
    letters = []
    seen = set()

    def add_letter(c, s, b):
        if (c, s, b) in seen:
            return
        else:
            seen.add((c, s, b))
        letters.append(c(s, b))

    for bottom_ending_class in bottom_endings:
        for consonant_class in consonants:
            add_letter(consonant_class, side_endings[0], bottom_ending_class)
    for side_ending_class in side_endings:
        for consonant_class in consonants:
            add_letter(consonant_class, side_ending_class, bottom_endings[0])

    #paper = typeset(letters, 100)
    paper = typeset_fixed(
        letters,
        letter_width=10,
        letters_per_line=10,
        #mode=StrokeMode(1.0),
        show_templates=True,
    )
    print(paper.format_svg(4))
