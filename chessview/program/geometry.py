import numpy as np
import math

class IntersectionError(Exception):
    pass

class Line:
    def __init__(self, rho_theta=None, points=None):
        self.rho = rho_theta[0] if rho_theta else None
        self.theta = rho_theta[1] if rho_theta else None
        if self.rho is not None and self.theta is not None:
            scale = 5000
            a = np.cos(self.theta)
            b = np.sin(self.theta)
            x0 = a*self.rho
            y0 = b*self.rho
            self.x1 = int(x0 + scale*(-b))
            self.y1 = int(y0 + scale*(a))
            self.x2 = int(x0 - scale*(-b))
            self.y2 = int(y0 - scale*(a))
        elif points is not None:
            (self.x1, self.y1, self.x2, self.y2) = points
        else:
            self.x1 = self.y1 = self.x2 = self.y2 = None

    def as_points(self):
        return [self.x1, self.y1, self.x2, self.y2]

def linesIntersection(line1, line2): 
    return intersectionPoint(line1.as_points(), line2.as_points())
    
def intersectionPoint(line1, line2):
    """Returns the intersection point of given lines.
    The intersection point returned as two values x,y.
    Each line is a list [ax, ay, bx, by]"""
    [ax1, ay1, ax2, ay2] = line1
    [bx1, by1, bx2, by2] = line2

    xdiff = (ax1 - ax2, bx1 - bx2)
    ydiff = (ay1 - ay2, by1 - by2)

    def det(a, b):
        return a[0] * b[1] - a[1] * b[0]

    div = det(xdiff, ydiff)
    if div == 0:
        raise IntersectionError('lines do not intersect')

    d = (det((ax1, ay1), (ax2, ay2)), det((bx1, by1), (bx2, by2)))
    x = det(d, xdiff) / div
    y = det(d, ydiff) / div
    return x, y
    
def visible_intersection_p(line1, line2, frame):
    try:
        frame_rows, frame_cols, _ = frame.shape
        x, y = linesIntersection(line1, line2)
        if x >= 0 and x <= frame_cols and y >= 0 and y <= frame_rows:
            return True
    except IntersectionError:
        pass
    return False

def distance(a, b):
    "Distance between two 2D-points."
    xdiff = a[0] - b[0]
    ydiff = a[1] - b[1]
    return math.sqrt(xdiff*xdiff + ydiff*ydiff)

###############################################################################################################################

def getOrderingAttribute(line):
    return 'x_value' if hasattr(line, 'x_value') else 'y_value'

def nextLine(lines, current, direction=+1, distance=1):
    if distance == 0: return current
    epsilon = 0.01
    attr = getOrderingAttribute(current)
    lines_order = lines if direction > 0 else reversed(lines)
    match = False
    for l in lines_order:
        if match:
            if distance <= 1:
                return l
            distance = distance - 1
        elif abs(getattr(l, attr) - getattr(current, attr)) < epsilon:
            match = True
    return None

def selectLines(lines, first, last):
    name = getOrderingAttribute(first)
    if getattr(first, name) >= getattr(last, name):
        first, last = last, first
    result = []
    for line in lines:
        if (getattr(first, name) <= getattr(line, name) and getattr(line, name) <= getattr(last, name)):
            result.append(line)
    return result



