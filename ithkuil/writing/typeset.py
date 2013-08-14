# TODO: boustrophedon
# TODO: half-character indent at right and left to indicate direction?

import sys
import traceback

from .common import (
    WIDTH, UNDER, BOTTOM, MIDDLE, TOP, OVER,
    CHAR_MARGIN, LINE_HEIGHT, PAGE_MARGIN
)
from canoepaddle import Pen, Paper
from canoepaddle.mode import StrokeOutlineMode, StrokeMode
from canoepaddle.bounds import Bounds


def typeset(
    letters,
    line_width=None,
    resolution=10,
    show_templates=False,
    show_bounds=False,
):
    """
    Arrange letters on the page.

    If line_width is None, then the line will not ever wrap.
    """
    paper = Paper()
    letter_bounds_list = []

    x = x_start = PAGE_MARGIN
    y = -PAGE_MARGIN

    for letter in letters:
        letter_paper = draw_letter(letter, show_template=show_templates)

        # Locate the letter on the page.
        bounds = letter_paper.bounds()
        bounds.bottom = UNDER
        bounds.top = OVER

        if show_bounds:
            pen = Pen()
            pen.fill_mode('#888')
            bounds.draw(pen)
            pen.paper.merge(letter_paper)
            letter_paper = pen.paper

        letter_paper.translate((-bounds.left, -OVER))
        letter_paper.translate((x, y))

        bounds.translate((x, y))
        letter_bounds_list.append(bounds)

        paper.merge(letter_paper)

        x += bounds.width + CHAR_MARGIN
        if line_width is not None and (x - x_start) > line_width:
            x = x_start
            y -= LINE_HEIGHT

    page_bounds = Bounds.union_all(letter_bounds_list)
    page_width = page_bounds.width + 2 * PAGE_MARGIN
    page_height = page_bounds.height + 2 * PAGE_MARGIN

    paper.set_pixel_size(resolution * page_width, resolution * page_height)
    paper.set_view_box(0, 0, page_width, -page_height)

    return paper


def typeset_fixed(
    letters,
    letter_width,
    letters_per_line=None,
    resolution=10,
    show_templates=False,
    show_bounds=False,
):
    """
    Arrange letters on the page in a grid.

    If letters_per_line is None, then the line will not ever wrap.
    """
    paper = Paper()
    letter_bounds_list = []

    x = x_start = PAGE_MARGIN + (letter_width / 2)
    y = -PAGE_MARGIN - OVER
    line_size = 0

    for letter in letters:
        letter_paper = draw_letter(letter, show_template=show_templates)

        # Locate the letter on the page.
        bounds = Bounds(-letter_width / 2, UNDER, +letter_width / 2, OVER)

        if show_bounds:
            pen = Pen()
            pen.fill_mode('#888')
            bounds.draw(pen)
            letter_paper.merge_under(pen.paper)

        letter_paper.translate((x, y))
        paper.merge(letter_paper)
        bounds.translate((x, y))
        letter_bounds_list.append(bounds)

        x += letter_width + CHAR_MARGIN
        line_size += 1
        if letters_per_line is not None and line_size >= letters_per_line:
            x = x_start
            y -= LINE_HEIGHT
            line_size = 0

    page_bounds = Bounds.union_all(letter_bounds_list)
    page_width = page_bounds.width + 2 * PAGE_MARGIN
    page_height = page_bounds.height + 2 * PAGE_MARGIN

    paper.set_pixel_size(resolution * page_width, resolution * page_height)
    paper.set_view_box(0, 0, page_width, -page_height)

    return paper


def draw_letter(letter, show_template=False):
    """
    Draw the given letter and return a Paper.

    The letter is located centered on x=0, and with y=0 as the
    character baseline.
    """
    print(str(letter), file=sys.stderr)

    letter_paper = Paper()

    if show_template:
        pen = Pen()
        pen.stroke_mode(0.125, '#88f')
        draw_template_path(pen)
        letter_paper.merge(pen.paper)

    pen = Pen()
    try:
        mode = StrokeOutlineMode(WIDTH, 0.2 * WIDTH, '#f51700', '#1d0603')
        #mode = StrokeMode(WIDTH, '#f51700')
        character_paper = letter.draw_character(mode)
    except Exception:
        traceback.print_exc()
    else:
        character_paper.center_on_x(0)
        letter_paper.merge(character_paper)

    return letter_paper


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
