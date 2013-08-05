import traceback

from canoepaddle import Pen, Paper

from ithkuil.writing.common import WIDTH, UNDER, BOTTOM, MIDDLE, TOP, OVER
from ithkuil.writing.consonant import consonants
from ithkuil.writing.bottom_ending import bottom_endings
from ithkuil.writing.side_ending import side_endings


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


def draw_letters(letters):
    resolution = 10  # Pixels / unit.
    char_width = 10
    char_height = 15
    page_width = 150
    page_height = 150
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

#DEBUG redefinition of parts lists for testing.
import ithkuil.writing.consonant as cons
#consonants = [cons.T, cons.K]
import ithkuil.writing.side_ending as side_ending
side_endings = [side_ending.Normal]
#side_endings = [side_ending.SideAll]
import ithkuil.writing.bottom_ending as bottom_ending
bottom_endings = [bottom_ending.Normal]
#bottom_endings = [bottom_ending.BottomAll]

if __name__ == '__main__':
    letters = []
    seen = set()

    def add_letter(c, s, b):
        if (c, s, b) in seen:
            return
        else:
            seen.add((c, s, b))
        letters.append(c(s, b))

    for consonant_class in consonants:
        for side_ending_class in side_endings:
            add_letter(consonant_class, side_ending_class, bottom_endings[0])
        for bottom_ending_class in bottom_endings:
            add_letter(consonant_class, side_endings[0], bottom_ending_class)

    paper = draw_letters(letters)
    print(paper.format_svg(3))
