import traceback

from .common import (
    WIDTH, UNDER, BOTTOM, MIDDLE, TOP, OVER,
)
from canoepaddle import Pen, Paper


def typeset(letters):
    resolution = 10  # Pixels / unit.
    char_width = 10
    char_height = 15
    page_width = char_width * 7 + 1
    page_height = 130
    x, y = x_start, y_start = 10, -14

    paper = Paper()
    paper.set_pixel_size(resolution * page_width, resolution * page_height)
    paper.set_view_box(0, 0, page_width, -page_height)

    for letter in letters:
        # Draw template.
        pen = Pen()
        pen.stroke_mode(0.125, '#88f')
        draw_template_path(pen)
        pen.paper.translate((x, y))
        paper.merge(pen.paper)

        # Draw letter.
        pen = Pen()
        try:
            pen.stroke_mode(WIDTH, '#f51700')
            letter.draw_character(pen)
            pen.outline_mode(WIDTH, 0.2, 'black')
            letter.draw_character(pen)
        except Exception:
            traceback.print_exc()
        pen.paper.center_on_x(0)
        pen.paper.translate((x, y))
        paper.merge(pen.paper)

        x += char_width
        if x >= (page_width - char_width):
            x = x_start
            y -= char_height

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
