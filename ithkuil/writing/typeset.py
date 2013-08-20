# TODO: boustrophedon
# TODO: half-character indent at right and left to indicate direction?

import sys
import traceback

from ithkuil.writing.common import (
    OVER, TOP, MIDDLE, BOTTOM, UNDER,
)
from canoepaddle import Pen, Paper

DEBUG_OUTPUT = True


def typeset(
    papers,
    letter_spacing=0,
    line_width=None,
    letters_per_line=None,
    line_spacing=0,
    page_margin=0,
    resolution=10,
):
    """
    Arrange letters on the page.

    Takes a list of paper objects.

    `letter_spacing` is how much space to leave between successive letters.
    `line_width` is how wide each line is before it wraps. A value of None
        means to not wrap based on this at all.
    `letters_per_line` is how many characters to place before wrapping. A
        value of None means to not wrap based on this at all.
    `line_spacing` is the vertical space left between lines.
    `page_margin` is how much space to leave around the edge of the page.
    `resolution` is the number of pixels per one unit.
    """
    # The letters are arranged into lines first, then the lines are arranged on
    # the page. The coordinates on each line have x=0 as the left edge, and y=0
    # as the text baseline. We trust each letter to be arranged correctly with
    # regard to the baseline.
    lines = [Paper()]
    letter_count = 0
    x = 0
    for letter_paper in papers:
        bounds = letter_paper.bounds()
        letter_paper.translate((-bounds.left, 0))

        # Start a new line if needed. Keep at least one letter per line.
        line_break = False
        if line_width is not None:
            if bounds.right + x > line_width:
                line_break = True
        if letters_per_line is not None:
            if letter_count >= letters_per_line:
                line_break = True
        if letter_count == 0:
            line_break = False

        if line_break:
            letter_count = 0
            x = 0
            lines.append(Paper())

        letter_paper.translate((x, 0))
        lines[-1].merge(letter_paper)

        letter_count += 1
        x += bounds.width + letter_spacing

    # Now we arrange completed lines on the page.
    page = Paper()
    y = 0
    for line in lines:
        bounds = line.bounds()
        line.translate((0, y))
        page.merge(line)
        y -= bounds.height + line_spacing

    # Set page margins and pixel size.
    page_bounds = page.bounds()
    page_bounds.left -= page_margin
    page_bounds.right += page_margin
    page_bounds.bottom -= page_margin
    page_bounds.top += page_margin
    page.override_bounds(page_bounds)

    return page


def draw_letter(
    letter,
    fixed_width=None,
    show_template=False,
    show_bounds=False,
    fuse=True,
):
    """
    Draw the given letter and return a Paper.

    The letter is located centered on x=0, and with y=0 as the
    character baseline.

    If `fixed_width` is specified, use that for the paper width.
    """
    if DEBUG_OUTPUT:
        print(str(letter), file=sys.stderr)

    try:
        character_paper = letter.draw_character(fuse=fuse)
    except Exception:
        if DEBUG_OUTPUT:
            traceback.print_exc()
            # Return an error pattern.
            pen = Pen()
            pen.fill_mode()
            pen.square(1)
            character_paper = pen.paper
        else:
            raise

    if fixed_width is not None:
        bounds = character_paper.bounds()
        bounds.left = -fixed_width / 2
        bounds.right = +fixed_width / 2
        character_paper.override_bounds(bounds)

    template_paper = Paper()
    if show_template:
        template_paper = draw_template_path()
    else:
        template_paper = Paper()

    letter_paper = Paper()
    letter_paper.merge(template_paper)
    letter_paper.merge(character_paper)

    # Set proper bounds for typesetting. Use the character bounds as our basis
    # so the template doesn't increase the size.
    bounds = character_paper.bounds()
    letter_paper.override_bounds(bounds)

    if show_bounds:
        pen = Pen()
        pen.fill_mode('#aaa')
        bounds.draw(pen)
        letter_paper.merge_under(pen.paper)

    return letter_paper


def draw_template_path():
    pen = Pen()
    pen.stroke_mode(0.05, '#466184')
    pen.turn_to(0)
    pen.move_to((0, BOTTOM))
    pen.line_forward(10)
    pen.move_to((0, MIDDLE))
    pen.line_forward(10)
    pen.move_to((0, TOP))
    pen.line_forward(10)
    pen.paper.center_on_x(0)
    return pen.paper
