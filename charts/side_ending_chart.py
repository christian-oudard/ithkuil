from ithkuil.writing.common import OVER, MIDDLE
from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset, draw_letter
import ithkuil.writing.consonant as cons

from canoepaddle.mode import OutlineMode

letters = [
    cons.SideEndingStub(side_ending, None)
    for side_ending in side_endings
]

papers = [
    draw_letter(
        letter,
        mode=OutlineMode(1.0, 0.2),
        #show_bounds=True,
    )
    for letter in letters
]

# Override bounds to be asymmetrical fixed width.
for paper in papers:
    bounds = paper.bounds()
    bounds.left = -2
    bounds.right = 5
    paper.override_bounds(bounds)

page = typeset(
    papers,
    letter_spacing=5.0,
    letters_per_line=9,
    line_spacing=7.0,
    page_margin=5.0,
)
print(page.format_svg(4))
