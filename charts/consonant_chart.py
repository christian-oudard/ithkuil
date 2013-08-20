from canoepaddle import Pen
from canoepaddle.mode import StrokeOutlineMode

from ithkuil.writing.consonant import consonants
from ithkuil.writing.typeset import draw_letter, typeset
from ithkuil.phonology import convert_ascii_to_html
import ithkuil.writing.side_ending as se
import ithkuil.writing.bottom_ending as be

font = 'Caudex'
font_size = 8
red = '#d6041a'
black = '#260003'
gray = '#233042'
mode = StrokeOutlineMode(1.0, 0.2, red, black)

papers = []
for consonant_class in consonants:
    letter = consonant_class(se.Normal, be.Normal)
    letter_paper = draw_letter(
        letter,
        mode=mode,
        fixed_width=27.0,
        show_template=True,
    )
    letter_paper.translate((5, 0), bounds=False)

    p = Pen()
    p.move_to((-5, 0))
    p.text(
        convert_ascii_to_html(consonant_class.pronunciation),
        8.0,
        'Caudex',
        '#233042',
        centered=True,
    )
    letter_paper.merge(p.paper)
    papers.append(letter_paper)

page = typeset(
    papers,
    letter_spacing=1.0,
    letters_per_line=6,
    line_spacing=3.5,
    page_margin=5.0,
)

print(page.format_svg(4, resolution=1000 / page.bounds().width))
