import traceback

from canoepaddle import Pen

from common import WIDTH, UNDER, BOTTOM, MIDDLE, TOP, OVER
from consonant import consonants
from bottom_ending import bottom_endings
from side_ending import side_endings

def draw_template_path(x, y):
    pen = Pen(offset=(x, y))
    pen.turn_to(0)
    pen.move_to((-1, UNDER))
    pen.stroke_forward(2)
    pen.move_to((-4, BOTTOM))
    pen.stroke_forward(8)
    pen.move_to((-4, MIDDLE))
    pen.stroke_forward(8)
    pen.move_to((-4, TOP))
    pen.stroke_forward(8)
    pen.move_to((-1, OVER))
    pen.stroke_forward(2)
    return pen.paper.to_svg_path(precision=5)

def draw_letters(letters):
    width = 10
    max_width = 190
    height = 15
    x, y = x_start, y_start = 10, -14
    character_path = []
    template_path = []

    for letter in letters:
        pen = Pen(offset=(x, y))
        pen.set_width(WIDTH)

        try:
            letter.draw_character(pen)
            character_path.append(pen.paper.to_svg_path_thick(precision=5))
        except Exception:
            traceback.print_exc()
        template_path.append(draw_template_path(x, y))

        x += width
        if x >= max_width:
            x = x_start
            y -= height

    return ''.join(character_path), ''.join(template_path)

#DEBUG redefinition of parts lists for testing.
import consonant as cons
#consonants = [cons.CHacek, cons.LCedilla, cons.Q, cons.G, cons.T, cons.D, cons.K, cons.RHacek, cons.L, cons.J]
import side_ending
side_endings = [side_ending.Normal]
import bottom_ending
#bottom_endings = [bottom_ending.Normal]
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

    character_path, template_path = draw_letters(letters)

    from string import Template
    with open('template.svg') as f:
        t = Template(f.read())
    output = t.substitute(
        character_path=character_path,
        template_path=template_path,
    )
    print(output)
