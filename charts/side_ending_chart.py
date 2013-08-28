from ithkuil.writing.side_ending import side_endings
from ithkuil.writing.typeset import typeset, draw_letter
import ithkuil.writing.consonant as cons

from canoepaddle import Pen, StrokeOutlineMode

font = 'Caudex'
font_size = 3
red = '#d6041a'
black = '#260003'
gray = '#233042'
mode = StrokeOutlineMode(1.0, 0.2, red, black)

letters = [
    cons.SideEndingStub(side_ending, None)
    for side_ending in side_endings
]

papers = []
for letter in letters:
    paper = draw_letter(letter, mode)

    # Override bounds to be asymmetrical fixed width.
    bounds = paper.bounds()
    bounds.left = -2
    bounds.right = 5
    bounds.bottom = 3
    if letter.side_ending_class.relation == 'UNFRAMED':
        bounds.top = 18
    else:
        bounds.top = 20
    paper.override_bounds(bounds)
    paper.translate((-0.5, 0))

    # Generate text indicating pattern and stem.
    pen = Pen()
    pen.paper = paper
    if letter.side_ending_class.relation == 'UNFRAMED':
        pen.move_to((2, 9.5))
    else:
        pen.move_to((2, 11.5))

    text = '{} {}'.format(
        letter.side_ending_class.pattern,
        letter.side_ending_class.stem,
    )
    pen.text(text, font_size, font, gray, centered=True)

    papers.append(paper)


page = typeset(
    papers,
    letter_spacing=4.0,
    letters_per_line=9,
    line_spacing=5.0,
    page_margin=5.0,
)

# Add row text.
page_bounds = page.bounds()
pen = Pen()
pen.paper = page
page_x_center = (page_bounds.left + page_bounds.right) / 2
pen.move_to((page_x_center, -3.0))
pen.text('Unframed Relation', font_size, font, gray, centered=True)
pen.move_to((page_x_center, -23.0))
pen.text('Framed Relation', font_size, font, gray, centered=True)

print(page.format_svg(4, resolution=1000 / page.bounds().width))
