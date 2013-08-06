# TODO: boustrophedon
# TODO: half-character indent at right and left to indicate direction?

import traceback

from .common import (
    WIDTH, UNDER, BOTTOM, MIDDLE, TOP, OVER,
    CHAR_MARGIN, LINE_HEIGHT, PAGE_MARGIN
)
from canoepaddle import Pen, Paper
from canoepaddle.mode import StrokeOutlineMode


def typeset(letters, line_width=None, resolution=10, show_templates=False):
    """
    Arrange letters on the page.

    If page width is None, then the line will not ever wrap.
    """
    paper = Paper()

    x = x_start = PAGE_MARGIN
    y = -PAGE_MARGIN

    for letter in letters:
        letter_paper = Paper()

        if show_templates:
            pen = Pen()
            pen.stroke_mode(0.125, '#88f')
            draw_template_path(pen)
            template_paper = pen.paper

        # Draw letter.
        pen = Pen()
        try:
            pen.set_mode(StrokeOutlineMode(WIDTH, 0.2 * WIDTH, '#f51700', '#1d0603'))
            #pen.stroke_mode(WIDTH, '#f51700')
            letter.draw_character(pen)
        except Exception:
            traceback.print_exc()
        pen.paper.center_on_x(0)
        letter_paper.merge(pen.paper)

        bounds = letter_paper.bounds()
        letter_paper = template_paper.merge(letter_paper)

        letter_paper.translate((-bounds.left, -OVER))
        letter_paper.translate((x, y))

        paper.merge(letter_paper)

        x += bounds.width + WIDTH + CHAR_MARGIN
        if line_width is not None and (x - x_start) > line_width:
            x = x_start
            y -= LINE_HEIGHT

    page_bounds = paper.bounds()
    page_width = page_bounds.width + 2 * PAGE_MARGIN
    page_height = page_bounds.height + 2 * PAGE_MARGIN

    paper.set_pixel_size(resolution * page_width, resolution * page_height)
    paper.set_view_box(0, 0, page_width, -page_height)

    return paper


def draw_template_path(pen):
    pen.move_to((0, 0))
    pen.turn_to(0)
    pen.move_to((-1, UNDER))
    pen.line_forward(2)
    pen.move_to((-4, BOTTOM))
    pen.line_forward(8)
    pen.move_to((-4, MIDDLE))
    pen.line_forward(8)
    pen.move_to((-4, TOP))
    pen.line_forward(8)
    pen.move_to((-1, OVER))
    pen.line_forward(2)
