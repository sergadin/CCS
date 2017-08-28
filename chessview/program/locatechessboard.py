import numpy as np
import cv2
import scipy
import scipy.spatial, scipy.spatial.distance
import scipy.cluster.hierarchy
from geometry import Line, distance, intersection_point, visible_intersection_p, lines_intersection
from painting import debug_painting
from grid import outerCorners, hintedBorder

import program.gvars as gvars

def auto_canny(image, sigma=0.33):
    # compute the median of the single channel pixel intensities
    v = np.median(image)
 
    # apply automatic Canny edge detection using the computed median
    lower = int(max(0, (1.0 - sigma) * v))
    upper = int(min(255, (1.0 + sigma) * v))
    edged = cv2.Canny(image, lower, upper)
 
    # return the edged image
    return edged
   
def find_representative_lines(image, lines, polarLines, threshold=15):
    if not lines or len(lines)==0:
        return []
    if len(lines) == 1:
        return polarLines
    height, width, point = image.shape
    X = lines
    def lines_proximity(line1, line2):
        pt1 = intersection_point(line1, [0, 0, width, height])
        pt2 = intersection_point(line2, [0, 0, width, height])
        return distance(pt1, pt2)
    
    def find_clusters(data, distanceFunction, threshold):
        """Returns two values:
        map from custer index to a list of data indeces,
        array that maps every data item to its cluster index."""
        linkage = scipy.cluster.hierarchy.linkage(data, metric=distanceFunction)
        clustering = scipy.cluster.hierarchy.fcluster(linkage, threshold, criterion='distance')
        clusters = {}
        for k in range(clustering.size):
            if not clusters.get(clustering[k]):
                clusters[clustering[k]] = []
            clusters[clustering[k]].append(k)
        return clusters, clustering    
        
    clusters, mapping = find_clusters(X, lines_proximity, threshold)
    representatives = []
    weighted_averages = [] # rho, theta, number of lines in the cluster, line object
    for clusterId, clusterMembers in clusters.items():
        members = [polarLines[k] for k in clusterMembers]
        thetas = [theta for rho, theta in members]
        rhos = [rho for rho, theta in members]
        weighted_averages.append( (sum(rhos)/len(rhos), sum(thetas)/len(thetas), len(rhos), Line(rho_theta=(rho, theta))) )
    # merge clusters with visible intersections
    #FIXME: this simple method will not merge a chain of pairwise intersecting lines
    merged = []
    for seq, (rho, theta, count, line) in enumerate(weighted_averages):
        if seq in merged:
            continue
        repr_rho, repr_theta, repr_count = rho*count, theta*count, count
        for seq2, (rho2, theta2, count2, line2) in enumerate(weighted_averages):
            if seq2 > seq and visible_intersection_p(line, line2, image):
                merged.append(seq2)
                repr_rho = repr_rho + rho2*count2
                repr_theta = repr_theta + theta2*count2
                repr_count = repr_count + count2
        representatives.append( (repr_rho/repr_count, repr_theta/repr_count) )
    return representatives

def find_squares(image, votes_per_line=125, raw_lines=False):
    thetas = []
    imageHL = image.copy()
    imageTemp = image.copy()
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    blurred = cv2.GaussianBlur(gray, (3, 3), 0)
 
    # apply Canny edge detection using automatically determined threshold
    auto = auto_canny(blurred)

    #Hough Tranform version1
    rad = np.pi/180
    lines = cv2.HoughLines(auto, 1, np.pi/180, votes_per_line)

    def polar_to_euclidian(rho, theta):
        a = np.cos(theta)
        b = np.sin(theta)
        x0 = a*rho
        y0 = b*rho
        x1 = int(x0 + 2000*(-b))
        y1 = int(y0 + 2000*(a))
        x2 = int(x0 - 2000*(-b))
        y2 = int(y0 - 2000*(a))
        return [x1, y1, x2, y2]

    def horizontal_line_p(rho, theta):
        return abs(theta-90*rad) < 30*rad

    verticalPolar = []
    horizontalPolar = []
    for (index, (rho, theta)) in enumerate(lines[0]): #this is houghLinesDetector resulit
        if horizontal_line_p(rho, theta):
            horizontalPolar.append((rho, theta))
        else:
            verticalPolar.append((rho, theta))

    #debug_painting(gvars.image_to_draw_121, [Line(rp) for rp in verticalPolar], [Line(rp) for rp in horizontalPolar])
    
    if not raw_lines:
        verticalPolar = find_representative_lines(image, [polar_to_euclidian(rho, theta) for rho, theta in verticalPolar], verticalPolar, threshold=30)
        horizontalPolar = find_representative_lines(image, [polar_to_euclidian(rho, theta) for rho, theta in horizontalPolar], horizontalPolar, threshold=30)

    vertical = [Line(rp) for rp in verticalPolar]
    horizontal = [Line(rp) for rp in horizontalPolar]

    #debug_painting(gvars.image_to_draw_121, vertical, horizontal)
        
    return vertical, horizontal 
   
   

def locateChessboard(frame, hint=None):
    "Returns board borders and transformation matrix for mapping image points to the unit square."
    frame_rows, frame_cols, _ = frame.shape
    
    def makeMidVerticalLine(frame):
        frame_rows, frame_cols, _ = frame.shape
        return Line(points=[frame_cols/2, 0, frame_cols/2, frame_rows])
    def makeMidHorizontalLine(frame):
        frame_rows, frame_cols, _ = frame.shape
        return Line(points=[0, frame_rows/2, frame_cols/2, frame_rows/2])
        
    mid_vertical_line = makeMidVerticalLine(frame)
    mid_horizontal_line = makeMidHorizontalLine(frame)
    
    vert, hor = find_squares(frame)
    
    if len(vert) < 2 or len(hor) < 2:
        return None, None, None
        
    # sort lines in increasing order
    for line in hor:
        line.y_value = lines_intersection(line, mid_vertical_line)[1]
    for line in vert:
        line.x_value = lines_intersection(line, mid_horizontal_line)[0]
    hor.sort(key=lambda line: line.y_value)
    vert.sort(key=lambda line: line.x_value)

    quality = None
    if hint is not None:
        board_borders, Minverse, quality = hintedBorder(frame, vert, hor, hint)
    else:
        board_borders, Minverse, quality = outerCorners(frame, vert, hor)

    print "locateChessboard(%s)" % (hint is None), Minverse
    return board_borders, Minverse, quality
    
    
