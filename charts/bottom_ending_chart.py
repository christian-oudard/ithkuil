from canoepaddle import *
from ithkuil.writing.common import BOTTOM
import ithkuil.writing.consonant as cons
import ithkuil.writing.bottom_ending as be
from ithkuil.writing.bottom_ending import bottom_endings
from ithkuil.writing.typeset import typeset, draw_letter

font = 'Caudex'
font_size = 3
red = '#d6041a'
black = '#260003'
gray = '#233042'
mode = StrokeOutlineMode(1.0, 0.2, red, black)

pairs = []
for bottom_ending_class in bottom_endings:
    pairs.append((
        cons.BottomEndingStraightStub(None, bottom_ending_class),
        cons.BottomEndingSlantedStub(None, bottom_ending_class),
    ))

papers = []
for straight_letter, slanted_letter in pairs:
    paper = Paper()
    straight_paper = draw_letter(straight_letter, mode)
    slanted_paper = draw_letter(slanted_letter, mode)

    # Adjust for scythe bottom ending.
    if straight_letter.bottom_ending_class == be.Scythe:
        for letter_paper in [straight_paper, slanted_paper]:
            bounds = letter_paper.bounds()
            bounds.right += 2.0
            letter_paper.override_bounds(bounds)

    # Assemble the two bottom endings onto one paper.
    paper.merge(straight_paper)
    offset_x = straight_paper.bounds().right - slanted_paper.bounds().left
    slanted_paper.translate((offset_x + 1.0, 0))
    paper.merge(slanted_paper)
    paper.center_on_x(0)
    papers.append(paper)

    # Draw the bottom line on each sheet.
    bounds = paper.bounds()
    pen = Pen()
    pen.stroke_mode(0.05, gray)
    pen.move_to((bounds.left - 2, BOTTOM))
    pen.line_to((bounds.right + 2, BOTTOM))
    paper.merge_under(pen.paper)

# Make the papers fixed width.
max_width = max(paper.bounds().width for paper in papers)
for paper in papers:
    bounds = paper.bounds()
    bounds.left = -max_width / 2
    bounds.right = max_width / 2
    paper.override_bounds(bounds)

page = typeset(
    papers,
    letter_spacing=2.0,
    letters_per_line=5,
    #line_width=100,
    line_spacing=3.0,
    page_margin=5.0,
)
print(page.format_svg(4, resolution=30))
#page.format_svg()
