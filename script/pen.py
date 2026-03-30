"""
Stroke-based rendering engine for Ithkuil script.

Inspired by canoepaddle: strokes are defined as centerlines with width,
and rendered as filled SVG paths with proper miter joins.

Coordinate system: Y-up (mathematical). SVG output flips Y.

Usage:
    paper = Paper()
    pen = Pen(paper)
    pen.set_width(1.0)
    pen.move_to(0, 0)
    pen.turn_to(0)  # face right
    pen.line_forward(5)
    pen.arc_left(90, 2)
    svg = paper.to_svg()
"""

import math
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from copy import copy, deepcopy
from collections import namedtuple


# ---------------------------------------------------------------------------
# Geometry helpers
# ---------------------------------------------------------------------------

def _rot(x, y, angle_rad):
    """Rotate vector (x, y) by angle_rad counterclockwise."""
    c, s = math.cos(angle_rad), math.sin(angle_rad)
    return x * c - y * s, x * s + y * c


def _perp(dx, dy):
    """Perpendicular to (dx, dy), rotated 90° CCW."""
    return -dy, dx


def _norm(x, y, length=1.0):
    """Scale vector to given length."""
    d = math.hypot(x, y)
    if d < 1e-12:
        return 0.0, 0.0
    return x / d * length, y / d * length


def _intersect_lines(p1, p2, p3, p4):
    """
    Intersection of infinite lines through p1-p2 and p3-p4.
    Returns (x, y) or None if parallel.
    """
    x1, y1 = p1
    x2, y2 = p2
    x3, y3 = p3
    x4, y4 = p4
    denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    if abs(denom) < 1e-12:
        return None
    t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
    return x1 + t * (x2 - x1), y1 + t * (y2 - y1)


def _intersect_circle_line(cx, cy, r, p1, p2):
    """
    Intersections of circle (cx, cy, r) with infinite line through p1, p2.
    Returns list of 0, 1, or 2 points.
    """
    dx, dy = p2[0] - p1[0], p2[1] - p1[1]
    fx, fy = p1[0] - cx, p1[1] - cy
    a = dx * dx + dy * dy
    if a < 1e-24:
        return []
    b = 2 * (fx * dx + fy * dy)
    c = fx * fx + fy * fy - r * r
    disc = b * b - 4 * a * c
    if disc < 0:
        return []
    sq = math.sqrt(max(0, disc))
    results = []
    for t in [(-b - sq) / (2 * a), (-b + sq) / (2 * a)]:
        results.append((p1[0] + t * dx, p1[1] + t * dy))
    return results


def _closest(ref, pts):
    """Return point in pts closest to ref."""
    return min(pts, key=lambda p: (p[0]-ref[0])**2 + (p[1]-ref[1])**2)


# ---------------------------------------------------------------------------
# Rich types for position and heading
# ---------------------------------------------------------------------------

class _PositionTuple(tuple):
    """A (x, y) tuple with .x and .y attribute access."""
    def __new__(cls, x, y):
        return tuple.__new__(cls, (float(x), float(y)))

    @property
    def x(self):
        return self[0]

    @property
    def y(self):
        return self[1]

    def __add__(self, other):
        return _PositionTuple(self[0] + other[0], self[1] + other[1])

    def __sub__(self, other):
        return _PositionTuple(self[0] - other[0], self[1] - other[1])


class Heading(float):
    """
    A heading in degrees (0=right, 90=up), with .rad, .flipped_x(), .flipped_y().
    Behaves as a float for arithmetic.
    """
    @property
    def rad(self):
        return math.radians(float(self))

    def flipped_x(self):
        """Reflect heading across X axis: angle → 180° - angle."""
        return Heading(180.0 - float(self))

    def flipped_y(self):
        """Reflect heading across Y axis: angle → -angle."""
        return Heading(-float(self))

    def __add__(self, other):
        return Heading(float(self) + float(other))

    def __sub__(self, other):
        return Heading(float(self) - float(other))

    def __neg__(self):
        return Heading(-float(self))

    def __repr__(self):
        return f'Heading({float(self)})'


# A Bounds object with .left/.right/.top/.bottom attributes.
class Bounds:
    """Bounding box with named attributes. Y-up coordinates."""
    def __init__(self, left, bottom, right, top):
        self.left   = float(left)
        self.bottom = float(bottom)
        self.right  = float(right)
        self.top    = float(top)

    def __iter__(self):
        yield self.left
        yield self.bottom
        yield self.right
        yield self.top

    def __repr__(self):
        return f'Bounds(left={self.left}, bottom={self.bottom}, right={self.right}, top={self.top})'

    @property
    def width(self):
        return self.right - self.left

    @property
    def height(self):
        return self.top - self.bottom


# ---------------------------------------------------------------------------
# Cap styles (not yet used beyond flat cap)
# ---------------------------------------------------------------------------

CAP_FLAT    = 'flat'
CAP_ROUND   = 'round'
CAP_POINTED = 'pointed'
CAP_CHISEL  = 'chisel'


# ---------------------------------------------------------------------------
# Segment: a single thick stroke primitive
# ---------------------------------------------------------------------------

@dataclass
class _Seg:
    """Thick stroke segment (line or arc)."""
    # Corner points of the filled shape (Y-up coords)
    a_left:  Optional[Tuple[float,float]] = None
    a_right: Optional[Tuple[float,float]] = None
    b_left:  Optional[Tuple[float,float]] = None
    b_right: Optional[Tuple[float,float]] = None
    width:   float = 1.0
    # True if this segment should NOT be joined with the previous one
    break_before: bool = False
    # Arc data for arc segments (None for line segments)
    arc_data: Optional[dict] = None
    # Centerline endpoints (for last_segment() geometry queries)
    a_center: Optional[Tuple[float,float]] = None
    b_center: Optional[Tuple[float,float]] = None


# ---------------------------------------------------------------------------
# Closed fill path (for hook shapes etc.)
# ---------------------------------------------------------------------------

class _ClosedFill:
    """
    A closed filled polygon composed of lines and circular arcs.
    Used for hook shapes that don't fit the thick-stroke model.
    """
    def __init__(self):
        # Commands: ('M', x, y), ('L', x, y), ('A', cx, cy, r, sa, ea), ('Z',)
        self._cmds = []

    def move_to(self, x, y):
        self._cmds.append(('M', float(x), float(y)))

    def line_to(self, x, y):
        self._cmds.append(('L', float(x), float(y)))

    def arc(self, cx, cy, r, start_angle, end_angle):
        """Add circular arc from start_angle to end_angle (radians)."""
        self._cmds.append(('A', float(cx), float(cy), float(r),
                           float(start_angle), float(end_angle)))

    def close(self):
        self._cmds.append(('Z',))

    def to_svg_path(self, p, n_per_90=8):
        """Render as SVG path data string (Y-flipped for SVG)."""
        parts = []
        for cmd in self._cmds:
            if cmd[0] == 'M':
                parts.append(f'M{_pt(cmd[1], cmd[2], p)}')
            elif cmd[0] == 'L':
                parts.append(f'L{_pt(cmd[1], cmd[2], p)}')
            elif cmd[0] == 'A':
                _, cx, cy, r, sa, ea = cmd
                arc_span = abs(ea - sa)
                n = max(4, int(arc_span / (math.pi / 2) * n_per_90))
                for i in range(1, n + 1):
                    a = sa + (ea - sa) * i / n
                    px_ = cx + r * math.cos(a)
                    py_ = cy + r * math.sin(a)
                    parts.append(f'L{_pt(px_, py_, p)}')
            elif cmd[0] == 'Z':
                parts.append('Z')
        return ' '.join(parts)

    def translate(self, dx, dy):
        new_cmds = []
        for cmd in self._cmds:
            if cmd[0] in ('M', 'L'):
                new_cmds.append((cmd[0], cmd[1] + dx, cmd[2] + dy))
            elif cmd[0] == 'A':
                _, cx, cy, r, sa, ea = cmd
                new_cmds.append(('A', cx + dx, cy + dy, r, sa, ea))
            else:
                new_cmds.append(cmd)
        self._cmds = new_cmds

    def mirror_x(self, x_center=0):
        new_cmds = []
        for cmd in self._cmds:
            if cmd[0] in ('M', 'L'):
                new_cmds.append((cmd[0], 2 * x_center - cmd[1], cmd[2]))
            elif cmd[0] == 'A':
                _, cx, cy, r, sa, ea = cmd
                # Mirror center x, flip arc direction, flip angles
                new_cx = 2 * x_center - cx
                new_sa = math.pi - sa
                new_ea = math.pi - ea
                new_cmds.append(('A', new_cx, cy, r, new_ea, new_sa))
            else:
                new_cmds.append(cmd)
        self._cmds = new_cmds

    def mirror_y(self, y_center=0):
        new_cmds = []
        for cmd in self._cmds:
            if cmd[0] in ('M', 'L'):
                new_cmds.append((cmd[0], cmd[1], 2 * y_center - cmd[2]))
            elif cmd[0] == 'A':
                _, cx, cy, r, sa, ea = cmd
                new_cy = 2 * y_center - cy
                new_sa = -sa
                new_ea = -ea
                new_cmds.append(('A', cx, new_cy, r, new_ea, new_sa))
            else:
                new_cmds.append(cmd)
        self._cmds = new_cmds

    def bounds(self):
        xs, ys = [], []
        for cmd in self._cmds:
            if cmd[0] in ('M', 'L'):
                xs.append(cmd[1])
                ys.append(cmd[2])
            elif cmd[0] == 'A':
                _, cx, cy, r, sa, ea = cmd
                # Sample arc for bounds
                n = max(4, int(abs(ea - sa) / (math.pi / 2) * 4))
                for i in range(n + 1):
                    a = sa + (ea - sa) * i / n
                    xs.append(cx + r * math.cos(a))
                    ys.append(cy + r * math.sin(a))
        if not xs:
            return Bounds(0, 0, 0, 0)
        return Bounds(min(xs), min(ys), max(xs), max(ys))


# ---------------------------------------------------------------------------
# Easing functions: t -> t' (both in [0, 1])
# Use these to shape how a value changes along a parametric stroke.
# ---------------------------------------------------------------------------

def ease_linear(t):
    return float(t)

def ease_in(t):
    """Slow start, fast finish (quadratic)."""
    return float(t) ** 2

def ease_out(t):
    """Fast start, slow finish (quadratic)."""
    t = float(t)
    return 1.0 - (1.0 - t) ** 2

def ease_in_out(t):
    """Slow at both ends (cubic S-curve)."""
    t = float(t)
    return 4 * t * t * t if t < 0.5 else 1.0 - (-2 * t + 2) ** 3 / 2

def ease_in_cubic(t):
    return float(t) ** 3

def ease_out_cubic(t):
    t = float(t)
    return 1.0 - (1.0 - t) ** 3


# ---------------------------------------------------------------------------
# Width profile constructors: return callable t -> float
# ---------------------------------------------------------------------------

def width_constant(w):
    """Uniform width along the stroke."""
    w = float(w)
    return lambda t: w


def width_taper(w_start, w_end, ease=None):
    """
    Width that interpolates from w_start to w_end, shaped by ease.
    ease defaults to ease_linear.
    """
    if ease is None:
        ease = ease_linear
    w_start, w_end = float(w_start), float(w_end)
    def fn(t):
        return w_start + (w_end - w_start) * ease(float(t))
    return fn


# ---------------------------------------------------------------------------
# Spine constructors: return callable t -> (x, y), t in [0, 1]
# ---------------------------------------------------------------------------

def spine_line(x1, y1, x2, y2):
    """Straight line spine from (x1,y1) to (x2,y2)."""
    x1, y1, x2, y2 = float(x1), float(y1), float(x2), float(y2)
    def fn(t):
        t = float(t)
        return (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
    return fn


def spine_arc(cx, cy, r, start_angle, end_angle):
    """
    Arc spine. cx, cy: center; r: radius.
    start_angle, end_angle: in radians (Y-up convention).
    Sweeps linearly from start_angle to end_angle as t goes 0→1.
    """
    cx, cy, r = float(cx), float(cy), float(r)
    sa, ea = float(start_angle), float(end_angle)
    def fn(t):
        a = sa + (ea - sa) * float(t)
        return (cx + r * math.cos(a), cy + r * math.sin(a))
    return fn


def spine_bezier(p0, p1, p2, p3):
    """
    Cubic Bezier spine. Each point is (x, y).
    p0/p3 are endpoints; p1/p2 are control points.
    """
    p0, p1, p2, p3 = [tuple(float(c) for c in p) for p in (p0, p1, p2, p3)]
    def fn(t):
        t = float(t)
        mt = 1.0 - t
        x = mt**3*p0[0] + 3*mt**2*t*p1[0] + 3*mt*t**2*p2[0] + t**3*p3[0]
        y = mt**3*p0[1] + 3*mt**2*t*p1[1] + 3*mt*t**2*p2[1] + t**3*p3[1]
        return (x, y)
    return fn


# ---------------------------------------------------------------------------
# Path: a sequence of segments rendered as one filled shape
# ---------------------------------------------------------------------------

class _Path:
    """A connected sequence of thick segments rendered as one filled outline."""

    def __init__(self):
        self.segs: List[_Seg] = []

    def append(self, seg: _Seg):
        self.segs.append(seg)

    def apply_joins(self):
        """Compute miter joins between consecutive segments."""
        for i in range(len(self.segs) - 1):
            a, b = self.segs[i], self.segs[i + 1]
            if a.b_left is None or b.a_left is None:
                continue

            a_is_arc = a.arc_data is not None
            b_is_arc = b.arc_data is not None

            if not a_is_arc and not b_is_arc:
                # Line-line: intersect offset lines
                p_left  = _intersect_lines(a.a_left, a.b_left, b.a_left, b.b_left)
                p_right = _intersect_lines(a.a_right, a.b_right, b.a_right, b.b_right)
            elif not a_is_arc and b_is_arc:
                # Line followed by arc: intersect line with arc's offset circles
                ad = b.arc_data
                p_left  = _intersect_circle_line_join(
                    ad['cx'], ad['cy'], ad['r_left'],  a.a_left, a.b_left, b.a_left)
                p_right = _intersect_circle_line_join(
                    ad['cx'], ad['cy'], ad['r_right'], a.a_right, a.b_right, b.a_right)
            elif a_is_arc and not b_is_arc:
                # Arc followed by line: intersect arc's offset circles with line
                ad = a.arc_data
                p_left  = _intersect_circle_line_join(
                    ad['cx'], ad['cy'], ad['r_left'],  b.a_left, b.b_left, a.b_left)
                p_right = _intersect_circle_line_join(
                    ad['cx'], ad['cy'], ad['r_right'], b.a_right, b.b_right, a.b_right)
            else:
                # Arc-arc: approximate as line-line at the join point
                p_left  = _intersect_lines(a.a_left, a.b_left, b.a_left, b.b_left)
                p_right = _intersect_lines(a.a_right, a.b_right, b.a_right, b.b_right)

            def forward(seg, pt, side='left'):
                src = seg.a_left if side == 'left' else seg.a_right
                dst = seg.b_left if side == 'left' else seg.b_right
                if src is None or dst is None or pt is None:
                    return False
                vx, vy = dst[0] - src[0], dst[1] - src[1]
                wx, wy = pt[0] - src[0], pt[1] - src[1]
                return vx * wx + vy * wy >= 0

            if p_left and forward(a, p_left, 'left') and forward(b, p_left, 'left'):
                a.b_left = b.a_left = p_left
            if p_right and forward(a, p_right, 'right') and forward(b, p_right, 'right'):
                a.b_right = b.a_right = p_right

    def to_svg_paths(self, precision: int = 3) -> List[str]:
        """Render each segment as a separate filled SVG path."""
        if not self.segs:
            return []
        paths = []
        for seg in self.segs:
            if seg.arc_data is not None:
                paths.append(_arc_seg_to_svg(seg, precision))
            else:
                paths.append(_line_seg_to_svg(seg, precision))
        return [p for p in paths if p]


def _intersect_circle_line_join(cx, cy, r, line_a, line_b, reference):
    """
    Find the intersection of a circle with a line that is closest to reference.
    Used for arc-line joins.
    """
    pts = _intersect_circle_line(cx, cy, r, line_a, line_b)
    if not pts:
        return _intersect_lines(line_a, line_b,
                                (cx, cy), (cx + 1, cy))  # fallback
    return _closest(reference, pts)


def _fmt(v, p):
    """Format float to p decimal places, stripping trailing zeros."""
    s = f'{v:.{p}f}'
    if '.' in s:
        s = s.rstrip('0').rstrip('.')
    return s


def _pt(x, y, p):
    """Format a point for SVG (Y is flipped)."""
    return f'{_fmt(x, p)},{_fmt(-y, p)}'


def _line_seg_to_svg(seg: _Seg, p: int) -> Optional[str]:
    """Filled quadrilateral for a line segment."""
    al, ar, bl, br = seg.a_left, seg.a_right, seg.b_left, seg.b_right
    if any(c is None for c in [al, ar, bl, br]):
        return None
    return f'M{_pt(*al,p)} L{_pt(*bl,p)} L{_pt(*br,p)} L{_pt(*ar,p)} Z'


def _arc_seg_to_svg(seg: _Seg, p: int, n_per_90: int = 8) -> Optional[str]:
    """
    Filled arc segment as polygon approximation.
    Left side (a_left→b_left) and right side (b_right→a_right) are
    approximated with n_per_90 points per 90° of arc.
    Endpoints use the actual corner points (which may be moved by joins).
    """
    al, ar, bl, br = seg.a_left, seg.a_right, seg.b_left, seg.b_right
    if any(c is None for c in [al, ar, bl, br]):
        return None
    d = seg.arc_data
    cx, cy = d['cx'], d['cy']
    r_left, r_right = d['r_left'], d['r_right']
    sa, ea = d['start_angle_rad'], d['end_angle_rad']

    arc_span = abs(ea - sa)
    n = max(4, int(arc_span / (math.pi / 2) * n_per_90))

    angles = [sa + (ea - sa) * i / n for i in range(n + 1)]
    left_pts  = [(cx + r_left  * math.cos(a), cy + r_left  * math.sin(a)) for a in angles]
    right_pts = [(cx + r_right * math.cos(a), cy + r_right * math.sin(a)) for a in angles]

    left_pts[0]  = al;  left_pts[-1]  = bl
    right_pts[0] = ar;  right_pts[-1] = br

    parts = [f'M{_pt(*al, p)}']
    for pt in left_pts[1:]:
        parts.append(f'L{_pt(*pt, p)}')
    parts.append(f'L{_pt(*br, p)}')
    for pt in reversed(right_pts[:-1]):
        parts.append(f'L{_pt(*pt, p)}')
    parts.append('Z')
    return ' '.join(parts)


# ---------------------------------------------------------------------------
# Paper: accumulates paths, exports SVG
# ---------------------------------------------------------------------------

class Paper:
    """Collects filled paths and exports them as SVG."""

    def __init__(self):
        self._paths: List[_Path] = []
        self._current: Optional[_Path] = None
        self._closed: List[_ClosedFill] = []  # closed fill shapes (hooks etc.)
        self._bounds_override: Optional[Bounds] = None

    def _ensure_path(self):
        if self._current is None:
            self._current = _Path()
            self._paths.append(self._current)

    def _add_seg(self, seg: _Seg):
        if seg.break_before:
            self._current = None
        self._ensure_path()
        self._current.append(seg)

    def add_closed(self, cf: _ClosedFill):
        """Add a pre-computed closed fill shape."""
        self._closed.append(cf)

    def finish(self):
        """Compute joins on all paths. Call before SVG export."""
        for path in self._paths:
            path.apply_joins()

    def to_svg(self, color='black', precision=3, width=None, height=None,
               viewbox=None) -> str:
        """Export all paths as an SVG string."""
        self.finish()
        parts = []
        for path in self._paths:
            for d in path.to_svg_paths(precision):
                parts.append(f'<path d="{d}" fill="{color}"/>')
        for cf in self._closed:
            d = cf.to_svg_path(precision)
            parts.append(f'<path d="{d}" fill="{color}"/>')
        content = '\n  '.join(parts)
        vb = viewbox or self._auto_viewbox(precision)
        w_attr = f' width="{width}"' if width else ''
        h_attr = f' height="{height}"' if height else ''
        return (
            f'<svg xmlns="http://www.w3.org/2000/svg"'
            f' viewBox="{vb}"{w_attr}{h_attr}>\n  '
            f'{content}\n</svg>'
        )

    def _auto_viewbox(self, p=1):
        """Compute viewBox from all corners."""
        xs, ys = [], []
        for path in self._paths:
            for seg in path.segs:
                for pt in [seg.a_left, seg.a_right, seg.b_left, seg.b_right]:
                    if pt:
                        xs.append(pt[0])
                        ys.append(-pt[1])  # Y-flipped for SVG
        for cf in self._closed:
            b = cf.bounds()
            xs.extend([b.left, b.right])
            ys.extend([-b.bottom, -b.top])
        if not xs:
            return '0 0 10 10'
        margin = 0.5
        x0 = min(xs) - margin
        y0 = min(ys) - margin
        w = max(xs) - min(xs) + 2 * margin
        h = max(ys) - min(ys) + 2 * margin
        return f'{_fmt(x0,p)} {_fmt(y0,p)} {_fmt(w,p)} {_fmt(h,p)}'

    def bounds(self) -> Bounds:
        """Return bounding box in Y-up coordinates."""
        xs, ys = [], []
        for path in self._paths:
            for seg in path.segs:
                for pt in [seg.a_left, seg.a_right, seg.b_left, seg.b_right]:
                    if pt:
                        xs.append(pt[0])
                        ys.append(pt[1])
        for cf in self._closed:
            b = cf.bounds()
            xs.extend([b.left, b.right])
            ys.extend([b.bottom, b.top])
        if not xs:
            return Bounds(0, 0, 0, 0)
        return Bounds(min(xs), min(ys), max(xs), max(ys))

    def override_bounds(self, bounds: Bounds):
        """Override bounds for typesetting (doesn't change actual geometry)."""
        self._bounds_override = bounds

    def typeset_bounds(self) -> Bounds:
        """Return bounds for typesetting (override if set, else actual)."""
        if self._bounds_override:
            return self._bounds_override
        return self.bounds()

    def center_on_x(self, x=0):
        """Translate so the horizontal center is at x."""
        b = self.bounds()
        if b.left == b.right:
            return
        cx = (b.left + b.right) / 2
        self.translate(x - cx, 0)

    def join_paths(self):
        """No-op: joins are already computed via apply_joins()."""
        pass

    def fuse_paths(self):
        """No-op: segments are rendered as separate filled shapes."""
        pass

    def merge(self, other: 'Paper'):
        """Append all paths from another paper."""
        for path in other._paths:
            for seg in path.segs:
                seg.break_before = True
                self._add_seg(seg)
        self._closed.extend(other._closed)
        self._current = None

    def translate(self, dx, dy):
        """Translate all paths in-place."""
        for path in self._paths:
            for seg in path.segs:
                for attr in ('a_left', 'a_right', 'b_left', 'b_right',
                             'a_center', 'b_center'):
                    pt = getattr(seg, attr)
                    if pt:
                        setattr(seg, attr, (pt[0] + dx, pt[1] + dy))
                if seg.arc_data:
                    seg.arc_data['cx'] += dx
                    seg.arc_data['cy'] += dy
        for cf in self._closed:
            cf.translate(dx, dy)

    def mirror_x(self, x_center=0):
        """Mirror all paths around x=x_center."""
        for path in self._paths:
            for seg in path.segs:
                for attr in ('a_left', 'a_right', 'b_left', 'b_right',
                             'a_center', 'b_center'):
                    pt = getattr(seg, attr)
                    if pt:
                        setattr(seg, attr, (2 * x_center - pt[0], pt[1]))
                seg.a_left, seg.a_right = seg.a_right, seg.a_left
                seg.b_left, seg.b_right = seg.b_right, seg.b_left
                if seg.arc_data:
                    d = seg.arc_data
                    d['cx'] = 2 * x_center - d['cx']
                    d['ccw'] = not d['ccw']
                    # Mirror r_left/r_right (swap because left/right flip)
                    d['r_left'], d['r_right'] = d['r_right'], d['r_left']
                    d['start_angle_rad'] = math.pi - d['start_angle_rad']
                    d['end_angle_rad']   = math.pi - d['end_angle_rad']
        for cf in self._closed:
            cf.mirror_x(x_center)

    def mirror_y(self, y_center=0):
        """Mirror all paths around y=y_center."""
        for path in self._paths:
            for seg in path.segs:
                for attr in ('a_left', 'a_right', 'b_left', 'b_right',
                             'a_center', 'b_center'):
                    pt = getattr(seg, attr)
                    if pt:
                        setattr(seg, attr, (pt[0], 2 * y_center - pt[1]))
                seg.a_left, seg.a_right = seg.a_right, seg.a_left
                seg.b_left, seg.b_right = seg.b_right, seg.b_left
                if seg.arc_data:
                    d = seg.arc_data
                    d['cy'] = 2 * y_center - d['cy']
                    d['ccw'] = not d['ccw']
                    d['r_left'], d['r_right'] = d['r_right'], d['r_left']
                    d['start_angle_rad'] = -d['start_angle_rad']
                    d['end_angle_rad']   = -d['end_angle_rad']
        for cf in self._closed:
            cf.mirror_y(y_center)

    def copy(self) -> 'Paper':
        return deepcopy(self)


# ---------------------------------------------------------------------------
# Pen: stateful drawing tool
# ---------------------------------------------------------------------------

class Pen:
    """
    Stateful drawing pen. Call pen methods to draw strokes; the resulting
    filled shapes are accumulated in pen.paper.

    Angles in degrees. 0 = right (+X), 90 = up (+Y).
    """

    def __init__(self, paper: Optional[Paper] = None):
        self.paper = paper if paper is not None else Paper()
        self._x = 0.0
        self._y = 0.0
        self._heading = 0.0   # degrees
        self._width = 1.0
        self._in_stroke = False

    # --- State accessors ---

    @property
    def position(self) -> _PositionTuple:
        return _PositionTuple(self._x, self._y)

    @property
    def heading(self) -> Heading:
        return Heading(self._heading)

    @property
    def width(self) -> float:
        return self._width

    def copy(self, paper=False) -> 'Pen':
        """
        Return a copy of this pen.

        By default (paper=False), the copy uses a fresh blank paper.
        If paper=True, the copy uses a deep copy of the current paper.
        """
        p = Pen(self.paper.copy() if paper else Paper())
        p._x = self._x
        p._y = self._y
        p._heading = self._heading
        p._width = self._width
        p._in_stroke = False  # copy starts a new stroke
        return p

    # --- Configuration ---

    def set_width(self, w: float):
        self._width = float(w)

    # Canoepaddle compatibility: mode object with .width
    @property
    def mode(self):
        return _ModeShim(self._width)

    def set_mode(self, mode):
        """Accept a mode-like object with .width; extract the width."""
        if hasattr(mode, 'width') and mode.width is not None:
            self._width = float(mode.width)

    # --- Movement (no drawing) ---

    def move_to(self, x, y=None):
        """Move to position without drawing. Accepts (x,y) or x,y."""
        if y is None:
            x, y = x
        self._x, self._y = float(x), float(y)
        self._in_stroke = False

    def move_forward(self, d: float):
        """Move forward by d without drawing."""
        hr = math.radians(self._heading)
        self._x += d * math.cos(hr)
        self._y += d * math.sin(hr)
        self._in_stroke = False

    def move_to_x(self, x: float):
        self._x = float(x)
        self._in_stroke = False

    def move_to_y(self, y: float):
        self._y = float(y)
        self._in_stroke = False

    # --- Turning ---

    def turn_to(self, angle):
        """Set heading to absolute angle (degrees)."""
        self._heading = float(angle)

    def turn_left(self, angle: float):
        self._heading += float(angle)

    def turn_right(self, angle: float):
        self._heading -= float(angle)

    def turn_toward(self, x, y=None):
        """Turn to face toward (x, y)."""
        if y is None:
            x, y = x
        dx, dy = float(x) - self._x, float(y) - self._y
        self._heading = math.degrees(math.atan2(dy, dx))

    # --- Stroke control ---

    def break_stroke(self):
        """Ensure the next segment starts a new path."""
        self._in_stroke = False

    # --- Segment query ---

    def last_segment(self) -> Optional[_Seg]:
        """Return the last drawn segment (with geometry attributes)."""
        for path in reversed(self.paper._paths):
            if path.segs:
                return path.segs[-1]
        return None

    def last_slant_width(self) -> float:
        """Return the cap width of the last drawn segment's end cap."""
        seg = self.last_segment()
        if seg is None or seg.b_left is None or seg.b_right is None:
            return self._width
        bl, br = seg.b_left, seg.b_right
        return math.hypot(bl[0] - br[0], bl[1] - br[1])

    # --- Drawing ---

    def line_forward(self, d: float, start_slant=None, end_slant=None):
        """Draw a line forward by distance d."""
        hr = math.radians(self._heading)
        x2 = self._x + d * math.cos(hr)
        y2 = self._y + d * math.sin(hr)
        self._draw_line(self._x, self._y, x2, y2,
                        start_slant=start_slant, end_slant=end_slant)
        self._x, self._y = x2, y2

    def line_to(self, x, y=None, start_slant=None, end_slant=None):
        """Draw a line to (x, y)."""
        if y is None:
            x, y = x
        x, y = float(x), float(y)
        self._draw_line(self._x, self._y, x, y,
                        start_slant=start_slant, end_slant=end_slant)
        dx, dy = x - self._x, y - self._y
        if abs(dx) > 1e-12 or abs(dy) > 1e-12:
            self._heading = math.degrees(math.atan2(dy, dx))
        self._x, self._y = x, y

    def line_to_x(self, x: float, end_slant=None, start_slant=None):
        """Draw a line to x=x, keeping current y."""
        self.line_to(float(x), self._y,
                     start_slant=start_slant, end_slant=end_slant)

    def line_to_y(self, y: float, end_slant=None, start_slant=None):
        """Draw a line to y=y, keeping current x."""
        self.line_to(self._x, float(y),
                     start_slant=start_slant, end_slant=end_slant)

    def arc_left(self, angle: float, radius: float):
        """Draw an arc turning left (CCW) by `angle` degrees."""
        self._draw_arc(angle, radius, left=True)

    def arc_right(self, angle: float, radius: float):
        """Draw an arc turning right (CW) by `angle` degrees."""
        self._draw_arc(angle, radius, left=False)

    def parametric_stroke(self, spine_fn, width_fn=None, n=16):
        """
        Draw a stroke along an arbitrary parametric spine.

        spine_fn:  callable t -> (x, y), t in [0, 1]
        width_fn:  callable t -> float (default: constant current width)
        n:         number of polygon segments to sample

        Advances the pen to the end of the spine.
        """
        if width_fn is None:
            width_fn = width_constant(self._width)

        ts = [i / n for i in range(n + 1)]
        pts = [spine_fn(t) for t in ts]
        ws  = [float(width_fn(t)) for t in ts]

        # Central-difference tangents
        tangents = []
        for i in range(len(pts)):
            if i == 0:
                dx = pts[1][0] - pts[0][0]
                dy = pts[1][1] - pts[0][1]
            elif i == n:
                dx = pts[n][0] - pts[n-1][0]
                dy = pts[n][1] - pts[n-1][1]
            else:
                dx = pts[i+1][0] - pts[i-1][0]
                dy = pts[i+1][1] - pts[i-1][1]
            length = math.hypot(dx, dy)
            if length < 1e-12:
                tangents.append((1.0, 0.0))
            else:
                tangents.append((dx / length, dy / length))

        # Left/right offset points
        left_pts  = []
        right_pts = []
        for (x, y), (tx, ty), w in zip(pts, tangents, ws):
            px, py = -ty, tx  # perpendicular (left of travel)
            hw = w / 2
            left_pts.append((x + hw * px, y + hw * py))
            right_pts.append((x - hw * px, y - hw * py))

        # Closed fill polygon: left side forward, right side backward
        cf = _ClosedFill()
        cf.move_to(*left_pts[0])
        for pt in left_pts[1:]:
            cf.line_to(*pt)
        for pt in reversed(right_pts):
            cf.line_to(*pt)
        cf.close()
        self.paper.add_closed(cf)

        # Advance pen
        self._x, self._y = pts[-1]
        self._heading = math.degrees(math.atan2(tangents[-1][1], tangents[-1][0]))
        self._in_stroke = False

    def arc_to(self, x, y=None, cx=None, cy=None, radius=None, clockwise=False):
        """
        Draw an arc to (x, y).

        With cx/cy: use explicit center.
        With radius: compute center from current heading.
        With neither: compute center from current heading (smooth arc).
        """
        if y is None:
            x, y = x
        x, y = float(x), float(y)

        if abs(x - self._x) < 1e-12 and abs(y - self._y) < 1e-12:
            return  # degenerate

        if cx is not None and cy is not None:
            cx, cy = float(cx), float(cy)
            r = math.hypot(self._x - cx, self._y - cy)
            cross = (self._x - cx) * (y - cy) - (self._y - cy) * (x - cx)
            left = cross > 0
            self._draw_arc_to_point(x, y, cx, cy, r, left=left)

        elif radius is not None:
            left = not clockwise
            hr = math.radians(self._heading)
            if left:
                cxc = self._x - radius * math.sin(hr)
                cyc = self._y + radius * math.cos(hr)
            else:
                cxc = self._x + radius * math.sin(hr)
                cyc = self._y - radius * math.cos(hr)
            self._draw_arc_to_point(x, y, cxc, cyc, abs(radius), left=left)

        else:
            # Smooth arc: center at intersection of perpendicular through start
            # and perpendicular bisector of chord.
            hr = math.radians(self._heading)
            perp_h = (-math.sin(hr), math.cos(hr))  # 90° CCW = left perp
            chord_dx, chord_dy = x - self._x, y - self._y

            # Line 1: through (x1,y1) in direction perp_h
            p1 = (self._x, self._y)
            p2 = (self._x + perp_h[0], self._y + perp_h[1])

            # Line 2: perpendicular bisector of chord
            mx, my = (self._x + x) / 2, (self._y + y) / 2
            p3 = (mx, my)
            p4 = (mx - chord_dy, my + chord_dx)

            ct = _intersect_lines(p1, p2, p3, p4)
            if ct is None:
                # Chord is parallel to heading → straight line
                self._draw_line(self._x, self._y, x, y)
                self._heading = math.degrees(math.atan2(chord_dy, chord_dx))
                self._x, self._y = x, y
                return

            cxc, cyc = ct
            r = math.hypot(self._x - cxc, self._y - cyc)
            # Determine direction: cross product of heading direction and chord
            cross = math.cos(hr) * chord_dy - math.sin(hr) * chord_dx
            left = cross > 0
            self._draw_arc_to_point(x, y, cxc, cyc, r, left=left)

    # --- Internal drawing helpers ---

    def _line_corners(self, x1, y1, x2, y2, w, start_slant, end_slant):
        """Compute the 4 corners of a line stroke with optional slanted caps."""
        dx, dy = x2 - x1, y2 - y1
        length = math.hypot(dx, dy)
        if length < 1e-12:
            return None, None, None, None

        ux, uy = dx / length, dy / length  # unit stroke direction
        px, py = -uy, ux                   # unit perpendicular (left)
        half_w = w / 2

        # Offset lines (parallel to stroke, offset by half_w)
        la = (x1 + half_w * px, y1 + half_w * py)
        lb = (x2 + half_w * px, y2 + half_w * py)
        ra = (x1 - half_w * px, y1 - half_w * py)
        rb = (x2 - half_w * px, y2 - half_w * py)

        def slant_intersect(base_x, base_y, slant):
            sv = _slant_vector(slant, px, py)
            sv_end = (base_x + sv[0], base_y + sv[1])
            left_pt  = _intersect_lines((base_x, base_y), sv_end, la, lb)
            right_pt = _intersect_lines((base_x, base_y), sv_end, ra, rb)
            if left_pt is None:
                left_pt  = (base_x + half_w * px, base_y + half_w * py)
            if right_pt is None:
                right_pt = (base_x - half_w * px, base_y - half_w * py)
            return left_pt, right_pt

        if start_slant is not None:
            a_left, a_right = slant_intersect(x1, y1, start_slant)
        else:
            a_left  = (x1 + half_w * px, y1 + half_w * py)
            a_right = (x1 - half_w * px, y1 - half_w * py)

        if end_slant is not None:
            b_left, b_right = slant_intersect(x2, y2, end_slant)
        else:
            b_left  = (x2 + half_w * px, y2 + half_w * py)
            b_right = (x2 - half_w * px, y2 - half_w * py)

        return a_left, a_right, b_left, b_right

    def _draw_line(self, x1, y1, x2, y2, start_slant=None, end_slant=None):
        """Add a line segment to the paper."""
        w = self._width
        a_left, a_right, b_left, b_right = self._line_corners(
            x1, y1, x2, y2, w, start_slant, end_slant)
        if a_left is None:
            return

        seg = _Seg(
            a_left=a_left, a_right=a_right,
            b_left=b_left, b_right=b_right,
            width=w,
            break_before=not self._in_stroke,
            a_center=(x1, y1),
            b_center=(x2, y2),
        )
        self.paper._add_seg(seg)
        self._in_stroke = True

    def _draw_arc(self, angle_deg: float, radius: float, left: bool):
        """Draw an arc turning left (CCW) or right (CW) by angle_deg degrees."""
        hr = math.radians(self._heading)
        if left:
            cx = self._x - radius * math.sin(hr)
            cy = self._y + radius * math.cos(hr)
        else:
            cx = self._x + radius * math.sin(hr)
            cy = self._y - radius * math.cos(hr)

        sa = math.atan2(self._y - cy, self._x - cx)
        ea = sa + math.radians(angle_deg) * (1 if left else -1)
        x2 = cx + radius * math.cos(ea)
        y2 = cy + radius * math.sin(ea)
        self._draw_arc_to_point(x2, y2, cx, cy, radius, left)

    def _draw_arc_to_point(self, x2, y2, cx, cy, radius, left):
        """Draw arc from current position to (x2, y2) around center (cx, cy)."""
        x1, y1 = self._x, self._y
        w = self._width
        r = abs(radius)
        half_w = w / 2

        start_angle = math.atan2(y1 - cy, x1 - cx)
        end_angle   = math.atan2(y2 - cy, x2 - cx)

        if left:   # CCW
            while end_angle <= start_angle:
                end_angle += 2 * math.pi
        else:      # CW
            while end_angle >= start_angle:
                end_angle -= 2 * math.pi

        r_inner = r - half_w
        r_outer = r + half_w

        def arc_pt(angle, radius_):
            return (cx + radius_ * math.cos(angle),
                    cy + radius_ * math.sin(angle))

        if left:
            a_left  = arc_pt(start_angle, r_inner)
            a_right = arc_pt(start_angle, r_outer)
            b_left  = arc_pt(end_angle,   r_inner)
            b_right = arc_pt(end_angle,   r_outer)
            r_left, r_right = abs(r_inner), r_outer
        else:
            a_left  = arc_pt(start_angle, r_outer)
            a_right = arc_pt(start_angle, r_inner)
            b_left  = arc_pt(end_angle,   r_outer)
            b_right = arc_pt(end_angle,   r_inner)
            r_left, r_right = r_outer, abs(r_inner)

        # end_heading: tangent direction at end of arc
        if left:
            end_h = math.degrees(end_angle) + 90  # CCW: tangent is +90° from radius
        else:
            end_h = math.degrees(end_angle) - 90  # CW: tangent is -90° from radius

        seg = _Seg(
            a_left=a_left, a_right=a_right,
            b_left=b_left, b_right=b_right,
            width=w,
            break_before=not self._in_stroke,
            arc_data={
                'cx': cx, 'cy': cy,
                'r_left': r_left, 'r_right': r_right,
                'start_angle_rad': start_angle,
                'end_angle_rad': end_angle,
                'ccw': left,
                'center': (cx, cy),
                'radius': r if left else -r,
                'end_heading': end_h,
            },
            a_center=(x1, y1),
            b_center=(x2, y2),
        )
        self.paper._add_seg(seg)
        self._in_stroke = True

        arc_span = abs(end_angle - start_angle)
        if left:
            self._heading += math.degrees(arc_span)
        else:
            self._heading -= math.degrees(arc_span)
        self._x, self._y = x2, y2


# ---------------------------------------------------------------------------
# Mode compatibility shim (for V3 code that uses pen.mode.width)
# ---------------------------------------------------------------------------

class _ModeShim:
    """Minimal mode-like object for compatibility with V3 reference code."""
    def __init__(self, width):
        self.width = width

    def copy(self):
        return _ModeShim(self.width)

    def outliner_mode(self):
        return self  # no separate fill mode; return self


# ---------------------------------------------------------------------------
# Slant vector helper
# ---------------------------------------------------------------------------

def _slant_vector(slant, px=None, py=None):
    """
    Compute the direction vector for a slant cap.

    slant: None (flat = perpendicular), float (degrees), or object with .rad.
    px, py: perpendicular unit vector (used when slant is None).
    Returns (vx, vy) - the direction of the cap line.
    """
    if slant is None:
        return px, py  # perpendicular cap (flat)
    if hasattr(slant, 'rad'):
        a = slant.rad
    else:
        a = math.radians(float(slant))
    return math.cos(a), math.sin(a)


# ---------------------------------------------------------------------------
# Paper factory functions
# ---------------------------------------------------------------------------

def mirror_x(paper: Paper, x_center=0) -> Paper:
    """Return a mirrored copy of paper (around x=x_center)."""
    p = paper.copy()
    p.mirror_x(x_center)
    return p


def mirror_y(paper: Paper, y_center=0) -> Paper:
    """Return a mirrored copy of paper (around y=y_center)."""
    p = paper.copy()
    p.mirror_y(y_center)
    return p
