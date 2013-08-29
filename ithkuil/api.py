import ithkuil.writing.consonant as consonant_module
import ithkuil.writing.primary as primary_module
import ithkuil.writing.bottom_ending as bottom_ending_module
import ithkuil.writing.side_ending as side_ending_module


def consonant(name, side, bottom):
    cons = getattr(consonant_module, name)
    if side is None:
        se = None
    else:
        se = getattr(side_ending_module, side)
    if bottom is None:
        be = None
    else:
        be = getattr(bottom_ending_module, bottom)
    return cons(se, be)


def primary(name, top, bottom):
    prim = getattr(primary_module, name)
    if top is None:
        te = None
    else:
        te = getattr(bottom_ending_module, top)
    if bottom is None:
        be = None
    else:
        be = getattr(bottom_ending_module, bottom)
    return prim(te, be)
