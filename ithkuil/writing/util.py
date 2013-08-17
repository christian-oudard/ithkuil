import math

import vec

from canoepaddle.geometry import intersect_circle_line, closest_point_to
from canoepaddle.heading import Heading, Angle


def hook(pen, slant_angle, arc_angle, distance):
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
    """
    slant_angle = Angle(slant_angle)
    arc_angle = Angle(arc_angle)

    old_mode = pen.mode
    hook_width = old_mode.width

    # The pen starts at the inside corner of the hook, facing along the
    # base of the hook.
    base_heading = pen.heading
    inside_corner = pen.position

    # Calculate the radius.
    circumference = distance / (arc_angle.theta / 360)
    radius = circumference / (2 * math.pi)

    # Trace the inside curve to find the hook tip and some other
    # important points.
    pen.turn_left(slant_angle)
    pen.arc_left(arc_angle, radius)
    inside_arc = pen.last_segment()
    pen.undo()

    tip = inside_arc.b
    inside_center = inside_arc.center
    inside_end_heading = inside_arc.end_heading

    # Calculate the outside corner position. The outside corner is located
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
    points = intersect_circle_line(
        inside_arc.center,
        abs(inside_arc.radius) + hook_width,
        inside_corner,
        vec.add(inside_corner, v_base),
    )
    # Take the intersection point that is "forward" from the inside corner.
    points = [
        p for p in points
        if vec.dot(
            vec.vfrom(inside_corner, p),
            v_base,
        ) > 0
    ]
    assert len(points) == 1
    outside_corner = points[0]

    if switch_corners:
        # If the corners are switched, we actually started the hook at the
        # outside corner, so we need to adjust.
        v_base_width = vec.vfrom(outside_corner, inside_corner)
        inside_corner = vec.add(inside_corner, v_base_width)
        outside_corner = vec.add(outside_corner, v_base_width)
        tip = vec.add(tip, v_base_width)
        inside_center = vec.add(inside_center, v_base_width)

    # Draw the hook.
    pen.set_mode(old_mode.outliner_mode())
    # Base.
    pen.move_to(inside_corner)
    pen.line_to(outside_corner)
    # Outer arc, tangent to outer circle.
    pen.turn_toward(inside_center)
    if arc_angle > 0:
        pen.turn_right(90)
    else:
        pen.turn_left(90)
    pen.arc_to(tip)
    # Inner arc.
    pen.turn_to(inside_end_heading + 180)
    pen.arc_to(inside_corner)

    ###DEBUG
    #pen.stroke_mode(0.07, '#aaa')

    #pen.move_to(inside_center)
    #pen.circle(0.2)
    #pen.circle(inside_arc.radius)
    #pen.circle(abs(inside_arc.radius) + hook_width)

    #for p in points:
        #pen.move_to(p)
        #pen.circle(0.2)

    #pen.move_to(inside_center)
    #pen.turn_toward(inside_arc.a)
    #pen.line_forward(10)

    #pen.move_to(inside_center)
    #pen.turn_toward(inside_arc.b)
    #pen.line_forward(10)

    #pen.move_to(inside_corner)
    #pen.turn_to(base_heading)
    #pen.move_forward(-5)
    #pen.line_forward(10)

    #v = vec.vfrom(outside_corner, inside_center)
    #v = vec.norm(v, hook_width)
    #inside_widest = vec.add(outside_corner, v)
    #pen.stroke_mode(0.07, '#afa')
    #pen.move_to(outside_corner)
    #pen.line_to(inside_widest)
    ###

    pen.set_mode(old_mode)
