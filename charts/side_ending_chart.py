from ithkuil.writing.common import OVER, MIDDLE
from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset, draw_letter
import ithkuil.writing.consonant as cons

from canoepaddle import Pen
from canoepaddle.mode import StrokeOutlineMode

letters = [
    cons.SideEndingStub(side_ending, None)
    for side_ending in side_endings
]

papers = []
for letter in letters:
    paper = draw_letter(
        letter,
        mode=StrokeOutlineMode(1.0, 0.2, '#c60416', '#260003'),
    )

    # Override bounds to be asymmetrical fixed width.
    bounds = paper.bounds()
    bounds.left = 0
    bounds.right = 7
    bounds.top = 18
    paper.override_bounds(bounds)
    paper.translate((-1.5, 0))

    # Generate text indicating pattern and stem.
    pen = Pen()
    pen.paper = paper
    if letter.side_ending.relation == 'UNFRAMED':
        pen.move_to((2, 9.5))
    else:
        pen.move_to((2, 11.5))
    pen.text(
        '{} {}'.format(
            letter.side_ending.pattern,
            letter.side_ending.stem,
        ),
        3.0,
        'Caudex',
        '#233042',
        centered=True,
    )

    papers.append(paper)


page = typeset(
    papers,
    letter_spacing=5.0,
    letters_per_line=9,
    line_spacing=7.0,
    page_margin=5.0,
)

# Add row text.
page_bounds = page.bounds()
pen = Pen()
pen.paper = page
page_x_center = (page_bounds.left + page_bounds.right) / 2
pen.move_to((page_x_center, 15))
pen.text(
    'Unframed Relation',
    3.0,
    'Caudex',
    '#233042',
    centered=True,
)
pen.move_to((page_x_center, -4))
pen.text(
    'Framed Relation',
    3.0,
    'Caudex',
    '#233042',
    centered=True,
)

print(page.format_svg(4))
