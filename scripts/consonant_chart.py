from canoepaddle import Pen
from canoepaddle.mode import StrokeOutlineMode

from ithkuil.writing.common import TOP, BOTTOM
from ithkuil.writing.consonant import consonants
from ithkuil.writing.typeset import draw_letter, typeset
from ithkuil.phonology import convert_ascii_to_html
import ithkuil.writing.side_ending as se
import ithkuil.writing.bottom_ending as be


papers = []
import ithkuil.writing.consonant as cons
consonants = [cons.P, cons.B]
for consonant_class in consonants:
    letter = consonant_class(se.Normal, be.Normal)
    letter_paper = draw_letter(
        letter,
        mode=StrokeOutlineMode(1.0, 0.1, '#c60416', '#3b0006'),
        fixed_width=20.0,
        show_template=True,
    )
    letter_paper.translate((3, 0), bounds=False)

    p = Pen()
    p.move_to((-5, 0))
    p.text(
        convert_ascii_to_html(consonant_class.pronunciation),
        TOP - BOTTOM,
        'Caudex',
        '#233042',
    )
    letter_paper.merge(p.paper)
    papers.append(letter_paper)

page = typeset(
    papers,
    letter_spacing=1.0,
    letters_per_line=6,
    line_spacing=2.0,
    page_margin=3.0,
)

print(page.format_svg(4))
