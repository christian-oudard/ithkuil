from ithkuil.writing.consonant import consonants
from ithkuil.writing.bottom_ending import bottom_endings
from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset, typeset_fixed


#DEBUG redefinition of parts lists for testing.
import ithkuil.writing.consonant as cons
#consonants = consonants[-6:]
consonants = [cons.CHacek, cons.LCedilla, cons.Q, cons.G, cons.D, cons.T, cons.K, cons.RHacek, cons.L, cons.J]
import ithkuil.writing.side_ending as side_ending
#side_endings = [side_ending.Normal, side_ending.RightOnBottom, side_ending.CurveDownOnBottom, side_ending.CurveUpOnBottom]#, side_ending.SideAll]
#side_endings = [side_ending.Normal, side_ending.SideAll]
side_endings = [side_ending.Normal]
import ithkuil.writing.bottom_ending as bottom_ending
bottom_endings = bottom_endings[4:9]
#bottom_endings = [bottom_ending.Normal]
#bottom_endings = [bottom_ending.Normal, bottom_ending.BottomAll]

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
    paper = typeset_fixed(letters, 10, 10, show_templates=True)
    print(paper.format_svg(4))
