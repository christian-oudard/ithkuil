#!/usr/bin/env python3
"""Tests for Ithkuil V4 script rendering pipeline.

Validates that formatives render correctly to SVG with proper character
types, counts, and structural elements.
"""
import sys, os, re, json
sys.path.insert(0, os.path.dirname(__file__))

from render import FormativeRenderer, render_word, render_consonant_cluster, draw_primary, draw_quaternary_case, draw_tertiary
from render_formative import render_from_json, render_sentence, CASE_MAP


def count_svg_elements(svg, tag):
    """Count occurrences of an SVG element type."""
    return len(re.findall(rf'<{tag}\b', svg))


def test_consonant_cluster_splitting():
    """Test that consonant clusters split into individual consonants correctly."""
    assert render_consonant_cluster('m') == ['m']
    assert render_consonant_cluster('ţř') == ['ţ', 'ř']
    assert render_consonant_cluster('kš') == ['k', 'š']
    assert render_consonant_cluster('rr') == ['r', 'r']
    assert render_consonant_cluster('l') == ['l']
    assert render_consonant_cluster('tx') == ['t', 'x']
    print('  consonant cluster splitting: OK')


def test_primary_character_elements():
    """Test that primary character generates expected SVG elements."""
    elems = draw_primary(0, 0, 50, 100)
    # Should have at least the diagonal bar polygon and context diacritic
    polygons = [e for e in elems if 'polygon' in e]
    assert len(polygons) >= 1, f"Expected polygon for diagonal bar, got {len(polygons)}"

    # EXS context should produce a diamond diacritic (polygon)
    elems_exs = draw_primary(0, 0, 50, 100, ctx='EXS')
    ctx_polys = [e for e in elems_exs if 'polygon' in e]
    assert len(ctx_polys) >= 2, "EXS should have bar polygon + diamond diacritic"

    # FNC context should produce a line diacritic
    elems_fnc = draw_primary(0, 0, 50, 100, ctx='FNC')
    ctx_lines = [e for e in elems_fnc if 'line' in e]
    assert len(ctx_lines) >= 1, "FNC should have line diacritic"

    # DYN function should add a parallel line
    elems_dyn = draw_primary(0, 0, 50, 100, func='DYN')
    dyn_lines = [e for e in elems_dyn if 'line' in e]
    elems_sta = draw_primary(0, 0, 50, 100, func='STA')
    sta_lines = [e for e in elems_sta if 'line' in e]
    assert len(dyn_lines) > len(sta_lines), "DYN should have more lines than STA"

    print('  primary character elements: OK')


def test_quaternary_case_types():
    """Test that all 8 case types produce different SVG output."""
    outputs = set()
    for ct in range(8):
        elems = draw_quaternary_case(0, 0, 50, 100, case_type=ct, case_num=1)
        svg = ''.join(elems)
        outputs.add(svg)
    assert len(outputs) == 8, f"Expected 8 distinct case types, got {len(outputs)}"
    print('  quaternary case types (8 distinct): OK')


def test_quaternary_case_numbers():
    """Test that all 9 case numbers produce different SVG output."""
    outputs = set()
    for cn in range(1, 10):
        elems = draw_quaternary_case(0, 0, 50, 100, case_type=0, case_num=cn)
        svg = ''.join(elems)
        outputs.add(svg)
    assert len(outputs) == 9, f"Expected 9 distinct case numbers, got {len(outputs)}"
    print('  quaternary case numbers (9 distinct): OK')


def test_tertiary_valence():
    """Test that different valences produce different output."""
    valences = ['MNO', 'PRL', 'CRO', 'RCP', 'CPL', 'DUP', 'DEM', 'CNG', 'PTI']
    outputs = set()
    for v in valences:
        elems = draw_tertiary(0, 0, 50, 100, valence=v)
        svg = ''.join(elems)
        outputs.add(svg)
    assert len(outputs) == 9, f"Expected 9 distinct valences, got {len(outputs)}"
    print('  tertiary valence (9 distinct): OK')


def test_renderer_word_structure():
    """Test that a complete word has the right character type sequence."""
    r = FormativeRenderer()
    r.add_primary()
    r.add_cluster(['m'])
    r.add_quaternary(case_type=0, case_num=1)
    svg = r.to_svg()

    # Should contain: polygon (primary bar), glyph path (secondary), line (quaternary stem)
    assert 'polygon' in svg, "Missing primary character bar"
    assert 'fill-rule="nonzero"' in svg, "Missing secondary character glyph"
    assert count_svg_elements(svg, 'line') >= 1, "Missing quaternary stem"
    print('  word structure (primary + secondary + quaternary): OK')


def test_renderer_cluster_gemination():
    """Test that geminated consonants render correctly."""
    r = FormativeRenderer()
    r.add_cluster(['r', 'r'])
    svg = r.to_svg()
    # Gemination should produce ONE glyph path + gemination tick marks
    glyph_count = svg.count('fill-rule="nonzero"')
    assert glyph_count == 1, f"Gemination should render 1 base glyph, got {glyph_count}"
    print('  consonant cluster gemination: OK')


def test_renderer_cluster_two_consonants():
    """Test that 2-consonant clusters render as base + extension."""
    r = FormativeRenderer()
    r.add_cluster(['ţ', 'ř'])
    svg = r.to_svg()
    # Should have 2 glyph paths: base character + bottom extension
    glyph_count = svg.count('fill-rule="nonzero"')
    assert glyph_count == 2, f"2-consonant cluster should render 2 paths, got {glyph_count}"
    print('  consonant cluster 2-consonant: OK')


def test_render_from_json():
    """Test the JSON-based rendering pipeline."""
    data = {
        "root": "m", "stem": 1, "func": "STA", "spec": "BSC", "ctx": "EXS",
        "case": "THM", "affixes": [{"cs": "ţř", "degree": 5, "type": 1, "slot": 5}]
    }
    svg = render_from_json(data)
    assert svg.startswith('<svg'), "Should produce valid SVG"
    assert 'polygon' in svg, "Should have primary character"
    assert 'fill-rule' in svg, "Should have secondary characters"
    print('  JSON rendering pipeline: OK')


def test_render_sentence():
    """Test multi-word sentence rendering."""
    words = [
        {"root": "m", "stem": 1, "func": "STA", "spec": "BSC", "ctx": "EXS", "case": "THM"},
        {"root": "l", "stem": 1, "func": "DYN", "spec": "BSC", "ctx": "EXS", "case": "ERG"},
    ]
    svg = render_sentence(words)
    assert svg.startswith('<svg'), "Should produce valid SVG"
    # Should have 2 primary characters (2 polygons for the bars)
    poly_count = count_svg_elements(svg, 'polygon')
    assert poly_count >= 4, f"2-word sentence needs >= 4 polygons (2 bars + 2 ctx), got {poly_count}"
    print('  sentence rendering: OK')


def test_case_map_coverage():
    """Test that CASE_MAP covers all expected cases."""
    assert len(CASE_MAP) >= 60, f"Expected >= 60 cases in CASE_MAP, got {len(CASE_MAP)}"
    # Check a few specific cases
    assert CASE_MAP['THM'] == (0, 1)
    assert CASE_MAP['ERG'] == (0, 7)
    assert CASE_MAP['LOC'] == (6, 1)
    print('  case map coverage: OK')


def test_degree_diacritics():
    """Test that all 9 degrees produce different marks."""
    from render import _draw_degree_diac
    outputs = set()
    for deg in range(1, 10):
        elems = []
        _draw_degree_diac(elems, 100, 100, deg)
        svg = ''.join(elems)
        outputs.add(svg)
    assert len(outputs) == 9, f"Expected 9 distinct degree marks, got {len(outputs)}"
    print('  degree diacritics (9 distinct): OK')


def run_all():
    print('Script rendering tests:')
    test_consonant_cluster_splitting()
    test_primary_character_elements()
    test_quaternary_case_types()
    test_quaternary_case_numbers()
    test_tertiary_valence()
    test_renderer_word_structure()
    test_renderer_cluster_gemination()
    test_renderer_cluster_two_consonants()
    test_render_from_json()
    test_render_sentence()
    test_case_map_coverage()
    test_degree_diacritics()
    print('All script rendering tests passed!')


if __name__ == '__main__':
    run_all()
