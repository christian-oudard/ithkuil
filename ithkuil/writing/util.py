import math

import vec

from canoepaddle.geometry import intersect_circle_line
from canoepaddle.heading import Angle


# TODO: make distance central, not on the inside edge.
def hook(pen, slant_angle, arc_angle, distance, adjust_inside=0, adjust_outside=0):
    """
    Draw a hook shape.

    Each hook has two arcs that meet at a point, with curvature in the
    same direction. They are connected at the other end by a line, creating two
    corners. The pen starts on the left corner, pointing toward the right
    corner. The width of the pen is also used as the maximum width of the hook.

    The `slant_angle` argument is 90 degrees for a hook with a "straight" base.
    For slanted bases toward the inside corner, 0 < slant_angle < 90.
    For slanted bases toward the outside corner, 90 < slant_angle < 180.

    `arc_angle` is the arc angle of the inside arc. If `arc_angle` is negative,
    then the hook curves to the right instead.

    `distance` is the arc length along the inside arc.

    `sharp_angle` adjusts the "sharpness" i.e. acuteness of the angle of the
    hook tip. Negative angles make it "blunter".
    """
    slant_angle = Angle(slant_angle)
    arc_angle = Angle(arc_angle)

    old_mode = pen.mode
    hook_width = old_mode.width


    # The pen starts at the "middle" of the hook, a point along the base that
    # is in the middle of the inside and outside bounding circles.
    base_heading = pen.heading
    base_middle = pen.position

    # Calculate the radius.
    circumference = distance / (arc_angle.theta / 360)
    radius = circumference / (2 * math.pi)

    # Trace the inside curve to find the hook tip and some other
    # important points.
    temp_pen = pen.copy()
    temp_pen.turn_left(slant_angle)
    temp_pen.arc_left(arc_angle, radius)
    center_arc = temp_pen.last_segment()

    tip = center_arc.b
    center = center_arc.center

    # Calculate the inside and outside corner position. The outside corner is located
    # along the base line, intersecting the outer circle. The outer circle is
    # concentric to the inner arc's circle, but with radius larger by the
    # hook_width.
    v_base = vec.rotate((1, 0), base_heading.rad)
    switch_corners = (
        (slant_angle > 0 and arc_angle < 0) or
        (slant_angle < 0 and arc_angle > 0)
    )

    if switch_corners:
        v_base = vec.neg(v_base)

    base_right = vec.add(base_middle, v_base)

    points_inside = intersect_circle_line(
        center, abs(center_arc.radius) - (hook_width / 2),
        base_middle, base_right,
    )
    points_outside = intersect_circle_line(
        center, abs(center_arc.radius) + (hook_width / 2),
        base_middle, base_right,
    )

    # Take the intersection point that is the most "forward" along the base
    # from the inside corner.
    def forwardness(p):
        v_p = vec.vfrom(base_middle, p)
        return vec.dot(v_p, v_base),

    inside_corner = max(points_inside, key=forwardness)
    outside_corner = max(points_outside, key=forwardness)

    # Adjust for having started in the middle of the hook. Move to where the
    # corner of the hook is at the pen start position.
    if not switch_corners:
        offset = vec.vfrom(inside_corner, base_middle)
    else:
        offset = vec.vfrom(outside_corner, base_middle)
    inside_corner = vec.add(inside_corner, offset)
    outside_corner = vec.add(outside_corner, offset)
    tip = vec.add(tip, offset)
    center = vec.add(center, offset)

    # Draw the hook.
    pen.set_mode(old_mode.outliner_mode())
    # Base.
    pen.move_to(inside_corner)
    pen.line_to(outside_corner)
    # Outer arc.
    pen.turn_toward(center)
    if arc_angle > 0:
        pen.turn_right(90 + adjust_inside)
    else:
        pen.turn_left(90 + adjust_inside)
    pen.arc_to(tip)
    # Inner arc.
    pen.turn_to(center_arc.end_heading + 180)
    if arc_angle > 0:
        pen.turn_right(adjust_outside)
    else:
        pen.turn_left(adjust_outside)

    pen.arc_to(inside_corner)

    pen.set_mode(old_mode)
