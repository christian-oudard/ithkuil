from ithkuil.writing.typeset import typeset
from ithkuil.writing.util import hook

from canoepaddle import Pen


def get_pen():
    p = Pen()
    p.outline_mode(2.0, 0.2, 'black')
    p.move_to((0, 0))
    p.turn_to(0)
    return p

papers = []

p = get_pen()
hook(p, 45, 90, 5.0)
papers.append(p.paper)

p = get_pen()
hook(p, 120, 75, 4.0)
papers.append(p.paper)

p = get_pen()
hook(p, 60, -75, 4.0)
papers.append(p.paper)

p = get_pen()
hook(p, 135, -90, 5.0)
papers.append(p.paper)

p = get_pen()
hook(p, -45, -90, 5.0)
papers.append(p.paper)

p = get_pen()
hook(p, -120, -75, 4.0)
papers.append(p.paper)

p = get_pen()
hook(p, -60, 75, 4.0)
papers.append(p.paper)

p = get_pen()
hook(p, -135, 90, 5.0)
papers.append(p.paper)

page = typeset(
    papers,
    letter_spacing=2.0,
    letters_per_line=4,
    line_spacing=2.0,
    page_margin=5.0,
)
print(page.format_svg(6, resolution=30))
