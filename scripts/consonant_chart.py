from canoepaddle.mode import StrokeOutlineMode

from ithkuil.writing.consonant import consonants
from ithkuil.writing.typeset import draw_letter, typeset
from ithkuil.phonology import convert_ascii
import ithkuil.writing.side_ending as se
import ithkuil.writing.bottom_ending as be


letters = []
for consonant_class in consonants:
    letters.append(consonant_class(se.Normal, be.Normal))

mode = StrokeOutlineMode(1.0, 0.1, '#c60416', '#3b0006')

papers = [
    draw_letter(
        letter,
        mode=mode,
        fixed_width=10.0,
        #show_template=True,
        show_bounds=True,
    )
    for letter in letters
]

page = typeset(
    papers,
    letter_spacing=1.0,
    letters_per_line=6,
    line_spacing=2.0,
    page_margin=3.0,
)

# Draw text.
# STUB, typeset papers with real letters

print(page.format_svg(4))
