"""Sanity tests for the V3 writing system."""
import sys, os, math
sys.path.insert(0, os.path.dirname(__file__))

from v3.consonant import consonants, consonants_by_pronunciation
from v3.side_ending import Normal as SNormal, RightOnBottom as SRightOnBottom
from v3.bottom_ending import (
    Normal as BNormal, Long as BLong,
    HookLeftOnRight, HookRightOnRight,
    bottom_endings, bottom_endings_by_pronunciation,
)
from v3.typeset import typeset_word


def test_all_consonants_render():
    """All 46 consonants render without error and produce finite bounds."""
    for cc in consonants:
        paper = cc(SNormal, BNormal).draw_character(width=0.5)
        paper.finish()
        b = paper.bounds()
        assert b is not None, f'{cc.__name__} bounds is None'
        for v in (b.left, b.right, b.top, b.bottom):
            assert math.isfinite(v), f'{cc.__name__} has non-finite bound: {b}'
        assert b.right > b.left, f'{cc.__name__} has zero width'
        assert b.top > b.bottom, f'{cc.__name__} has zero height'
    print(f'test_all_consonants_render OK ({len(consonants)} consonants)')


def test_consonants_centered():
    """All consonants are approximately x-centered (bounding box spans x=0)."""
    for cc in consonants:
        paper = cc(SNormal, BNormal).draw_character(width=0.5)
        b = paper.bounds()
        # After center_on_x(0), center should be close to 0
        cx = (b.left + b.right) / 2
        assert abs(cx) < 0.6, f'{cc.__name__} not centered: cx={cx:.2f}, bounds={b}'
    print('test_consonants_centered OK')


def test_mirrored_consonants():
    """Mirrored consonants have horizontally flipped bounds vs their originals."""
    from v3.consonant import P, B, T, D, K, G, S, Z
    pairs = [(P, B), (T, D), (K, G), (S, Z)]
    for orig, mirror in pairs:
        bp = orig(SNormal, BNormal).draw_character(width=0.5).bounds()
        bm = mirror(SNormal, BNormal).draw_character(width=0.5).bounds()
        # Width should be similar
        wp = bp.right - bp.left
        wm = bm.right - bm.left
        assert abs(wp - wm) < 0.5, f'{orig.__name__}/{mirror.__name__} width mismatch: {wp:.2f} vs {wm:.2f}'
    print('test_mirrored_consonants OK')


def test_side_endings():
    """Side endings with hook-on-right produce wider characters."""
    from v3.consonant import K
    normal_paper = K(SNormal, BNormal).draw_character(width=0.5)
    hooked_paper = K(SRightOnBottom, BNormal).draw_character(width=0.5)
    nb = normal_paper.bounds()
    hb = hooked_paper.bounds()
    # RightOnBottom side ending adds width to the right
    assert hb.right > nb.right, 'RightOnBottom should extend right'
    print('test_side_endings OK')


def test_bottom_endings_by_pronunciation():
    """Lookup table maps pronunciation strings to bottom ending classes."""
    assert bottom_endings_by_pronunciation.get('l-') is BLong
    assert 'k-' in bottom_endings_by_pronunciation
    assert 'p-' in bottom_endings_by_pronunciation
    print(f'test_bottom_endings_by_pronunciation OK ({len(bottom_endings_by_pronunciation)} entries)')


def test_bottom_endings_render():
    """All bottom endings render without error."""
    from v3.consonant import K, T  # K has bottom_straight=False, T has bottom_straight=True
    for be in bottom_endings:
        for cc in (K, T):
            try:
                paper = cc(SNormal, be).draw_character(width=0.5)
                paper.finish()
                b = paper.bounds()
                assert b is not None
                for v in (b.left, b.right, b.top, b.bottom):
                    assert math.isfinite(v), f'{be.__name__} on {cc.__name__} has non-finite bound'
            except Exception as e:
                raise AssertionError(f'{be.__name__} on {cc.__name__} failed: {e}') from e
    print(f'test_bottom_endings_render OK ({len(bottom_endings)} endings x 2 consonants)')


def test_typeset_word():
    """typeset_word assembles multiple characters left-to-right."""
    from v3.consonant import K, S, T
    chars = [
        (K, SNormal, BNormal),
        (S, SNormal, BNormal),
        (T, SNormal, BNormal),
    ]
    paper = typeset_word(chars, width=0.5)
    b = paper.bounds()
    assert b is not None
    # Word should be wider than any single character
    single = K(SNormal, BNormal).draw_character(width=0.5).bounds()
    w_single = single.right - single.left
    w_word = b.right - b.left
    assert w_word > w_single * 2, f'word too narrow: {w_word:.2f} vs single {w_single:.2f}'
    print(f'test_typeset_word OK (word width={w_word:.2f})')


def test_consonants_by_pronunciation():
    """Lookup by pronunciation works."""
    from v3.consonant import P, B, K
    assert consonants_by_pronunciation['p'] is P
    assert consonants_by_pronunciation['b'] is B
    assert consonants_by_pronunciation['k'] is K
    assert consonants_by_pronunciation["k'"] is not None
    print(f'test_consonants_by_pronunciation OK ({len(consonants_by_pronunciation)} entries)')


if __name__ == '__main__':
    tests = [
        test_all_consonants_render,
        test_consonants_centered,
        test_mirrored_consonants,
        test_side_endings,
        test_bottom_endings_by_pronunciation,
        test_bottom_endings_render,
        test_typeset_word,
        test_consonants_by_pronunciation,
    ]
    failures = []
    for t in tests:
        try:
            t()
        except Exception as e:
            import traceback
            print(f'FAIL {t.__name__}: {e}')
            traceback.print_exc()
            failures.append(t.__name__)
    print()
    if failures:
        print(f'FAILED: {failures}')
        sys.exit(1)
    else:
        print(f'All {len(tests)} tests passed.')
