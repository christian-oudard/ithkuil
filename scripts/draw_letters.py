from ithkuil.writing.consonant import consonants
from ithkuil.writing.bottom_ending import bottom_endings
from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset, draw_letter

from canoepaddle.mode import OutlineMode

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

    papers = [
        draw_letter(
            letter,
            mode=OutlineMode(1.0, 0.1),
            fixed_width=10.0,
            show_template=True,
            #show_bounds=True,
        )
        for letter in letters
    ]
    page = typeset(
        papers,
        letter_spacing=2.0,
        letters_per_line=10,
        page_margin=5.0,
    )
    print(page.format_svg(6))
