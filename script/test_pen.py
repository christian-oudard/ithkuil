"""Quick sanity tests for pen.py stroke rendering."""
import sys, os
sys.path.insert(0, os.path.dirname(__file__))

from pen import Paper, Pen

def save(paper, name):
    svg = paper.to_svg(color='black', precision=2)
    path = f'/tmp/{name}.svg'
    with open(path, 'w') as f:
        f.write(svg)
    print(f'Saved {path}')
    return svg

def test_line():
    """Simple horizontal line."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(2)
    pen.move_to(0, 0)
    pen.turn_to(0)
    pen.line_forward(10)
    save(p, 'test_line')
    assert 'path' in p.to_svg()
    print('test_line OK')

def test_l_shape():
    """L-shape: line right then line up, joined."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(0, 0)
    pen.turn_to(0)
    pen.line_forward(5)
    pen.turn_left(90)
    pen.line_forward(5)
    svg = save(p, 'test_l_shape')
    # Should produce 2 path elements (one per segment)
    count = svg.count('<path')
    print(f'  path count: {count}')
    assert count == 2, f'expected 2 paths, got {count}'
    print('test_l_shape OK')

def test_arc_left():
    """Quarter arc turning left (CCW)."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(0, 0)
    pen.turn_to(0)
    pen.arc_left(90, 5)
    svg = save(p, 'test_arc_left')
    assert 'path' in svg
    # center at (0,5), start_angle=-90°, sweep +90° => end at (5,5), heading 90°
    print(f'  heading after arc: {pen.heading:.1f} (expected 90)')
    assert abs(pen.heading - 90) < 0.01
    print(f'  position after arc: ({pen._x:.2f}, {pen._y:.2f}) (expected ~5, 5)')
    assert abs(pen._x - 5) < 0.01 and abs(pen._y - 5) < 0.01
    print('test_arc_left OK')

def test_arc_right():
    """Quarter arc turning right (CW)."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(0, 0)
    pen.turn_to(0)
    pen.arc_right(90, 5)
    svg = save(p, 'test_arc_right')
    assert 'path' in svg
    # center at (0,-5), start_angle=90°, sweep -90° => end at (5,-5), heading -90°
    print(f'  heading after arc: {pen.heading:.1f} (expected -90)')
    assert abs(pen.heading - (-90)) < 0.01
    print(f'  position after arc: ({pen._x:.2f}, {pen._y:.2f}) (expected ~5, -5)')
    assert abs(pen._x - 5) < 0.01 and abs(pen._y - (-5)) < 0.01
    print('test_arc_right OK')

def test_line_then_arc():
    """Line then arc, joined."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(0, 0)
    pen.turn_to(0)
    pen.line_forward(5)
    pen.arc_left(90, 3)
    svg = save(p, 'test_line_then_arc')
    count = svg.count('<path')
    print(f'  path count: {count}')
    assert count == 2
    print('test_line_then_arc OK')

def test_full_circle():
    """Full 360° arc."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(5, 0)
    pen.turn_to(90)
    pen.arc_left(360, 5)
    svg = save(p, 'test_full_circle')
    assert 'path' in svg
    print('test_full_circle OK')

def test_break_stroke():
    """Two separate strokes."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(0, 0)
    pen.turn_to(0)
    pen.line_forward(5)
    pen.break_stroke()
    pen.move_to(0, 3)
    pen.line_forward(5)
    svg = save(p, 'test_break_stroke')
    count = svg.count('<path')
    print(f'  path count: {count}')
    assert count == 2
    print('test_break_stroke OK')

def test_translate():
    """Paper translation."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(0, 0)
    pen.turn_to(0)
    pen.line_forward(4)
    b1 = p.bounds()
    p.translate(10, 5)
    b2 = p.bounds()
    print(f'  bounds before: {b1}')
    print(f'  bounds after translate(10,5): {b2}')
    assert abs(b2.left - (b1.left + 10)) < 0.01
    assert abs(b2.bottom - (b1.bottom + 5)) < 0.01
    print('test_translate OK')

def test_mirror_x():
    """Paper x-mirror."""
    p = Paper()
    pen = Pen(p)
    pen.set_width(1)
    pen.move_to(1, 0)
    pen.turn_to(0)
    pen.line_forward(2)
    p2 = p.copy()
    p2.mirror_x(0)
    b1, b2 = p.bounds(), p2.bounds()
    print(f'  original bounds: {b1}')
    print(f'  mirrored bounds: {b2}')
    assert abs(b2.right + b1.left) < 0.01, f'{b2.right} vs {b1.left}'
    print('test_mirror_x OK')

if __name__ == '__main__':
    tests = [
        test_line,
        test_l_shape,
        test_arc_left,
        test_arc_right,
        test_line_then_arc,
        test_full_circle,
        test_break_stroke,
        test_translate,
        test_mirror_x,
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
