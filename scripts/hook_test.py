from ithkuil.writing.typeset import typeset
from ithkuil.writing.util import hook

from canoepaddle import Pen


def get_pen():
    p = Pen()
    p.outline_mode(2.0, 0.2, 'black')
    p.move_to((0, 0))
    p.turn_to(0)
    return p

def draw(
    pen,
    slant_angle,
    arc_angle,
    distance,
    adjust_inside,
    adjust_outside,
):
    old_pos = pen.position
    old_heading = pen.heading
    hook(pen, slant_angle, arc_angle, distance)
    pen.move_to(old_pos)
    pen.turn_to(old_heading)
    hook(
        pen, slant_angle, arc_angle, distance, adjust_inside=adjust_inside,
        adjust_outside=adjust_outside,
    )

papers = []

distance = 5.0
for slant_angle in [60, -60, 120, -120]:
    for arc_angle in [75, -75]:
        for adjust_inside in [0, 30]:
            for adjust_outside in [0, 10]:
                pen = get_pen()
                draw(
                    pen,
                    slant_angle,
                    arc_angle,
                    distance,
                    adjust_inside,
                    adjust_outside,
                )
                papers.append(pen.paper)

page = typeset(
    papers,
    letter_spacing=2.0,
    letters_per_line=4,
    #line_spacing=2.0,
    page_margin=5.0,
)
print(page.format_svg(6, resolution=30))
