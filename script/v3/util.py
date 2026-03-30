"""
Hook utility for V3 writing system.

A hook is a filled comma/wedge shape: a tapered arc stroke that starts
at full pen width and tapers to a point. The pen's current position is
the base of the hook (the wide end).

The spine is a circular arc parameterized by t in [0, 1]. The width
profile tapers from full width at t=0 to zero at t=1, shaped by an
easing function so that the taper accelerates near the tip.
"""

import math
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from pen import spine_arc, width_taper, ease_out


def hook(pen, slant_angle, arc_angle, distance, adjust_inside=0, adjust_outside=0):
    """
    Draw a hook shape attached to the pen's current position.

    slant_angle: degrees to turn from pen heading before starting the arc
    arc_angle:   total sweep in degrees (positive=CCW, negative=CW)
    distance:    arc length; combined with arc_angle to determine radius
    adjust_inside/adjust_outside: accepted for API compatibility, unused
    """
    hook_width = pen.width
    arc_angle_rad = math.radians(abs(arc_angle))
    if arc_angle_rad < 1e-9:
        return
    radius = distance / arc_angle_rad

    # Starting direction = pen heading rotated by slant_angle
    start_heading_rad = math.radians(float(pen.heading) + slant_angle)

    # Arc center: left of start direction for CCW, right for CW
    if arc_angle > 0:  # CCW
        cx = pen._x - radius * math.sin(start_heading_rad)
        cy = pen._y + radius * math.cos(start_heading_rad)
    else:              # CW
        cx = pen._x + radius * math.sin(start_heading_rad)
        cy = pen._y - radius * math.cos(start_heading_rad)

    start_angle = math.atan2(pen._y - cy, pen._x - cx)
    sweep = abs(arc_angle_rad)
    end_angle = start_angle + (sweep if arc_angle > 0 else -sweep)

    s_fn = spine_arc(cx, cy, radius, start_angle, end_angle)
    w_fn = width_taper(hook_width, 0.0, ease=ease_out)

    # Hook is a decorative appendage: save and restore pen state
    saved = (pen._x, pen._y, pen._heading, pen._in_stroke)
    pen.parametric_stroke(s_fn, w_fn)
    pen._x, pen._y, pen._heading, pen._in_stroke = saved
