from ithkuil.writing.consonant import consonants
from ithkuil.writing.bottom_ending import bottom_endings
from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset, draw_letter

from canoepaddle.mode import *

#DEBUG redefinition of parts lists for testing.
import ithkuil.writing.consonant as cons
import ithkuil.writing.side_ending as se
import ithkuil.writing.bottom_ending as be


letters = []
for width in (0.4, 0.6, 0.8, 1.0, 1.2, 1.4):
    mode = StrokeMode(width)
    letters.append(cons.Q(se.DiagonalDownRightOnTop, be.HookLeftOnRight, mode))

papers = [
    draw_letter(letter)
    for letter in letters
]
page = typeset(
    papers,
    letter_spacing=2.0,
    letters_per_line=6,
    line_spacing=1.0,
    page_margin=5.0,
)
print(page.format_svg(6))
