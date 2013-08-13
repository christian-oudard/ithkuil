from ithkuil.writing.consonant import consonants
from ithkuil.writing.bottom_ending import bottom_endings
from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset


#DEBUG redefinition of parts lists for testing.
import ithkuil.writing.consonant as cons
consonants = [
    cons.P, cons.B,
    cons.T, cons.D,
    cons.K, cons.G,
    cons.Q, cons.RHacek,
    cons.C, cons.ZDot,
    cons.CHacek, cons.J,
    cons.L, cons.LCedilla,
    cons.H, cons.Stop,
    cons.PStop, cons.PH,
][-4:]
#consonants = [cons.CHacek, cons.LCedilla, cons.Q, cons.G, cons.D, cons.T, cons.K, cons.RHacek, cons.L, cons.J]
import ithkuil.writing.side_ending as side_ending
#side_endings = [side_ending.Normal, side_ending.RightOnBottom, side_ending.CurveDownOnBottom, side_ending.CurveUpOnBottom]#, side_ending.SideAll]
#side_endings = [side_ending.Normal, side_ending.SideAll]
side_endings = [side_ending.Normal]
import ithkuil.writing.bottom_ending as bottom_ending
#bottom_endings = [bottom_ending.Normal]
#bottom_endings = [bottom_ending.Normal, bottom_ending.BottomAll]
bottom_endings = bottom_endings[:3]

if __name__ == '__main__':
    letters = []
    seen = set()

    def add_letter(c, s, b):
        if (c, s, b) in seen:
            return
        else:
            seen.add((c, s, b))
        letters.append(c(s, b))

    for consonant_class in consonants:
        for side_ending_class in side_endings:
            add_letter(consonant_class, side_ending_class, bottom_endings[0])
        for bottom_ending_class in bottom_endings:
            add_letter(consonant_class, side_endings[0], bottom_ending_class)

    paper = typeset(letters, 100, show_templates=True)
    print(paper.format_svg(4))
