from canoepaddle import StrokeMode
from ithkuil.writing.consonant import consonants
from ithkuil.writing.typeset import typeset, draw_letter

import ithkuil.writing.side_ending as se
import ithkuil.writing.bottom_ending as be
consonants = [c for c in consonants if not c.mirrored_x]

letters = []
for width in [0.6, 1.0, 1.2]:
    mode = StrokeMode(width)
    for consonant_class in consonants:
        letters.append(consonant_class(se.SideAll, be.BottomAll, mode))

papers = [
    draw_letter(
        letter,
        fixed_width=10.0,
        show_template=True,
        #show_bounds=True,
        fuse=False,
    )
    for letter in letters
]
page = typeset(
    papers,
    letter_spacing=2.0,
    letters_per_line=len(consonants),
    line_spacing=3.0,
    page_margin=5.0,
)
print(page.format_svg(4, resolution=30))
