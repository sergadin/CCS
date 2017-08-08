import numpy as np
import math
import cv2
from geometry import distance, nextLine, selectLines, linesIntersection
import gvars as gvars
from painting import paintArrayPoints, drawLine, display_hint
from matricies import makeTransformationMatricies, makeInverseMatrix
    
    
def numpyize(value):
    return np.array(value).astype("float32")

###############################################################################################################################

def generateIdealBoard(num_lines=9):
    "Generates a num_lines-by-num_lines matrix representing ideal board (unit square)"
    ideal_board_as_list = []
    for row in range(num_lines):
        ideal_row = np.zeros((num_lines, 2), dtype = "float32")
        row_x = float(row)/float(num_lines-1)
        for k in range(num_lines):
            ideal_row[k] = np.array([float(k)/float(num_lines-1), row_x])
        ideal_board_as_list.append( ideal_row )
    ideal_board = np.array( ideal_board_as_list )
    return ideal_board
    
###############################################################################################################################


def gridAlignmentQuality(grid, pattern, hm, vm):
    "Calculate quality of pattern alignment. HM[k] is the grid's line number for pattern's line k."
    quality = 0
    count = 0
    distances = []
    dmat = []
    for (pattern_line, grid_line) in enumerate(hm):
        dmat_row = []
        if grid_line is None: continue
        for (pattern_col, grid_col) in enumerate(vm):
            if grid_col is None: continue
            distances.append( 10*distance((pattern[pattern_line][pattern_col][0], pattern[pattern_line][pattern_col][1]),
                                          (grid[grid_line][grid_col][0], grid[grid_line][grid_col][1])) )
                                          
            dmat_row.append(distances[-1])
            count = count + 1
        dmat.append(dmat_row)
    if count == 0: return 10000 # infinity
    d = np.array(distances)
    
    def coverageDegree(count):
        return 1 - math.log(count/81.0 + 1, 2)
        
    return round(np.median(d) + 250*coverageDegree(count), 5) ##   (1+np.mean(d))*(81-count) good-1
    
    
        
def bestPatternAlignment(grid, pattern):
    """Find best position of a grid-like pattern in the given
    grid. Both grid and pattern are matrices of 2d-points in ideal
    space. Pattern is a rectangualar array.

    Return value:
    two lists of length of the patten that map grid lines to pattern's lines.
    """
    gridHorSize, gridVertSize, _ = grid.shape
    patternHorSize, patternVertSize, _ = pattern.shape
    ideal_board_square_size = 1 / 8.0
    matching_delta = ideal_board_square_size / 5.0

    def closest(v):
        if v < 0: return (0, abs(v))
        if v > 1: return (8, abs(1-v))
        #idx = min(8, int(round(v / ideal_board_square_size, 1)))
        idx = 0
        for i in range(9):
            if abs(v-ideal_board_square_size*i) < abs(v-ideal_board_square_size*idx):
                idx = i
        return (idx, abs(v - ideal_board_square_size*idx))

    hor_best_indices = []
    for i in range(0, gridHorSize):
        ideal_ys = [grid[i][j][1] for j in range(gridVertSize)]
        #print i, np.mean(ideal_ys), i*ideal_board_square_size, closest(np.mean(np.array(ideal_ys)))
        hor_best_indices.append( closest(np.mean(np.array(ideal_ys))) )
    hor_best_indices = [((idx, dist) if dist < matching_delta else (None, 1)) for (idx, dist) in hor_best_indices]

    vert_best_indices = []
    for j in range(0, gridVertSize):
        ideal_xs = [grid[i][j][0] for i in range(gridHorSize)]
        vert_best_indices.append( closest(np.mean(np.array(ideal_xs))) )
    vert_best_indices = [((idx, dist) if dist < matching_delta else (None, 1)) for (idx, dist) in vert_best_indices]

    def makeMatchings(best_indices):
        def expand(items):
            if len(items) == 0: return []
            all_tails = expand(items[1:])
            if all_tails == []: return [[i] for i in items[0]]
            if len(items[0]) == 0: return [[None] + tail for tail in all_tails]
            result = []
            for i in items[0]:
                result.extend( [[i] + tail for tail in all_tails] )
            return result
        def insert_nones(matchings, required_length=9):
            "Equalize length of matchings by adding Nones at the end."
            return [matching + [None for k in range(required_length-len(matching))] for matching in matchings]
            
        not_none_indices = [i for i, x in best_indices if i is not None]
        if not_none_indices == []: return insert_nones(not_none_indices)
            
        candidates_for_pattern_line = [[] for pl in range(max(not_none_indices)+1)]
        for grid_index, (best_pattern_index, distance) in enumerate(best_indices):
            if best_pattern_index is not None:
                candidates_for_pattern_line[best_pattern_index].append(grid_index)
        return insert_nones(expand(candidates_for_pattern_line))

    vert_matchings = makeMatchings(vert_best_indices)
    hor_matchings = makeMatchings(hor_best_indices)
    
    #print "------ ####### ------"
    ## print grid.shape
    #print "hor=", hor_best_indices, "vert=", vert_best_indices
    ## print hor_matchings
    best_choice = (None, None)
    best_quality = None
    for hor_m in hor_matchings:
        for vert_m in vert_matchings:
            quality = gridAlignmentQuality(grid, pattern, hor_m, vert_m)
            #print "quality=", quality, "choice=", (hor_m, vert_m)
            if best_quality is None or quality < best_quality:
                best_quality = quality
                best_choice = (hor_m, vert_m)
    ## print "------"
    ## print grid
    ## print "quality=", best_quality, "choice=", best_choice
    #if len([x for x in vert_best_indices if x is not None]) > 7:
        #print hor_matchings, vert_matchings, "==>", best_quality
        #print hor_best_indices, vert_best_indices, "==>", best_quality
        #print
        #pass
    return (best_quality or 10000), best_choice

###############################################################################################################################
###############################################################################################################################




def matchGridPattern(grid, pattern=None):
    
    if pattern == None: pattern = generateIdealBoard()
    
    gridHorSize, gridVertSize, _ = grid.shape
    patternHorSize, patternVertSize, _ = pattern.shape
       
    def makeRectangle(grid, n_hor, n_vert, width=1, heigth=1):
        gridHorSize, gridVertSize, _ = grid.shape
        if (n_hor < 0 or n_hor >= gridHorSize or n_hor+width < 0 or n_hor+width >= gridHorSize or
           n_vert < 0 or n_vert >= gridVertSize or n_vert+heigth < 0 or n_vert+heigth >= gridVertSize):
           return None
        return [grid[n_hor][n_vert], grid[n_hor][n_vert+heigth], grid[n_hor+width][n_vert+heigth], grid[n_hor+width][n_vert]]
       
    def paintSquarePoint(square):
        for x, y in square:
            cv2.circle(gvars.image_to_draw_121, (x, y), radius=5, color=[255,200,190], thickness=2)
        #cv2.imshow('Total', gvars.image_to_draw_121)        
        #cv2.waitKey(0)
                
    def countBadnessPoints(pattern, frame, M_to_frame, test):
        frameHorSize, frameVertSize, _ = frame.shape
        patternHorSize, patternVertSize, _ = pattern.shape
        
        board = cv2.perspectiveTransform(pattern, M_to_frame)
        count = 0
        for i in range(patternHorSize):
            for j in range(patternVertSize):
                x, y = board[i][j]
                if not test(frame, x, y):
                    count = count + 1
        return count
    
    def countInvisiblePoints(pattern, frame, M_to_frame):
        def point_on_frame_p(frame, x, y):
            frameHorSize, frameVertSize, _ = frame.shape
            return (x >= 0 and x < frameVertSize and y >= 0 and y < frameHorSize)
        return countBadnessPoints(pattern, frame, M_to_frame, point_on_frame_p)
        
    def countPointsOnWhite(pattern, frame, M_to_frame):
        def point_on_white_p(frame, x, y):
            frameHorSize, frameVertSize, _ = frame.shape
            x = int(x)
            y = int(y)
            return (x >= 0 and x < frameVertSize and y >= 0 and y < frameHorSize) and (np.std(frame[y][x])>10 or np.sum(frame[y][x])<200)
        return countBadnessPoints(pattern, frame, M_to_frame, point_on_white_p)
        
        
    best_quality = None
    best_matr = None
    best_square = None
    best_matr_to_ideal = None
    best_alignment = (None, None)
    best_conf = (None, None)
    best_quality_wh = None 
    
    rectangle_config = [(x, x) for x in range(1, 9) if (x%2==0 or x==1)]
    for wid, heig in reversed(rectangle_config):
        updated = False
        for green_number in range(gridHorSize * gridVertSize):
            green_line, green_column = int(green_number / gridVertSize), green_number % gridVertSize
            square = makeRectangle(grid, green_line, green_column, width=wid, heigth=heig)
            if square is None:
                continue
            for yellow_number in range(patternHorSize * patternVertSize):
                yellow_line, yellow_column = int(yellow_number / patternVertSize), yellow_number % patternHorSize
                ideal_square = makeRectangle(pattern, yellow_line, yellow_column, width=wid, heigth=heig)
                if ideal_square is None:
                    continue
                #paintSquarePoint(square) 
                M_to_ideal = cv2.getPerspectiveTransform(numpyize(square), numpyize(ideal_square))
                M_to_frame = cv2.getPerspectiveTransform(numpyize(ideal_square), numpyize(square))
                #board = cv2.perspectiveTransform(numpyize(pattern), M_to_frame)
                #paintArrayPoints(board)
                idealized_green_points = cv2.perspectiveTransform(numpyize(grid), M_to_ideal)
                quality, (hor_alignment, vert_alignment) = bestPatternAlignment(idealized_green_points, pattern)
                
                quality = quality + 0.001*countInvisiblePoints(pattern, gvars.image_to_draw_121, M_to_frame) + 0.001*countPointsOnWhite(pattern, gvars.image_with_original_colors, M_to_frame)
                
                if best_quality is None or (quality < best_quality and quality > 0): # Exclude unbelievably good results 
                    best_square = square
                    best_quality = quality  
                    best_matr = M_to_frame
                    best_matr_to_ideal = M_to_ideal
                    best_alignment = (hor_alignment, vert_alignment)
                    best_conf = (wid, heig)
                    updated = True
                
        if (best_quality is not None and best_quality<0.06) or (max(wid, heig) <=3 and best_quality is not None and best_quality_wh is not None and not updated):
            break
            
        print (wid, heig), best_quality_wh, "-->", best_quality
        best_quality_wh = best_quality
        
    print "Good enough by ", best_conf
    board = cv2.perspectiveTransform(numpyize(pattern), best_matr)
    paintSquarePoint(best_square)
    paintArrayPoints(board, color=[200,100,90], radius=8)            
    return best_quality, best_alignment, best_matr_to_ideal
    
###############################################################################################################################

def makeGrid(vert, hor, dimention=2):
    "Intersection points in frame-coordinates."
    for line in vert + hor:
        drawLine(gvars.image_to_draw_121, line, color=(120, 120, 220), width=2)

    grid = np.zeros((len(hor), len(vert), dimention), dtype="float32")
    for (i, h) in enumerate(hor):
        for (j, v) in enumerate(vert):
            x, y = linesIntersection(v, h)
            grid[i][j] = np.array([x, y]).astype("float32")
    return grid   
    
    
    
###############################################################################################################################

def mappings_to_borders(vert, hor, vert_m, hor_m):
    def minMaxLines(values, returnIndices=None):
        arr = [x for x in values if x is not None]
        if returnIndices:                     ### is min index equal index of min elem? Yes!
            arr = [i for i, x in enumerate(values) if x is not None]   
        return min(arr), max(arr) 

    left, right = minMaxLines(vert_m)
    top, bottom = minMaxLines(hor_m)
    
    border_lines = {"left": vert[left], "right": vert[right], "bottom": hor[bottom], "top": hor[top]}
    left_i, right_i = minMaxLines(vert_m, returnIndices=True)
    top_i, bottom_i = minMaxLines(hor_m, returnIndices=True)

    return border_lines, (left_i, right_i, top_i, bottom_i)
    
    
def hintedBorder(frame, vert, hor, hint):
    "Hint is frame to ideal matrix from previous frame."
    n_vert, n_hor = len(vert), len(hor)
    frame_to_ideal = hint
    ideal_board = generateIdealBoard()
    ideal_board_size = ideal_board.shape[0]

    hv_corners = makeGrid(vert, hor)
    idealized_corners = cv2.perspectiveTransform(hv_corners, frame_to_ideal)

    # TODO: rotation, movement and scaling (?) of the frame

    quality, (hor_m, vert_m) = bestPatternAlignment(idealized_corners, ideal_board)
    
    if quality is None or hor_m is None or vert_m is None:
        return None, None, None

    if quality > 200:
        print "hintedBorder, quality > 200" #, idealized_corners
        
    display_hint(frame, vert, hor, hint, vert_m=vert_m, hor_m=hor_m)

    border_lines, (left_i, right_i, top_i, bottom_i) = mappings_to_borders(vert, hor, vert_m, hor_m)
    points = [[left_i, top_i], [right_i, top_i], [right_i, bottom_i], [left_i, bottom_i]]
    points = [[x/8., y/8.] for [x, y] in points]
    
    to_current_frame, from_current_frame = makeTransformationMatricies(border_lines, points=points)

    return border_lines, from_current_frame, quality



###############################################################################################################################
###############################################################################################################################


def outerCorners(frame, vert, hor):
    "Find lines constituting the largest completely visible rectangle."

    def find_first_line(lines_to_search, test):
        for line in lines_to_search:
            if test(line):
                return line
        return None

    def number_of_visible_intersections(line, other_lines, frame):
        cnt = 0
        for line2 in other_lines:
            try:
                if visible_intersection_p(line, line2, frame):
                    cnt = cnt + 1
            except Exception:
                pass
        return cnt

    def guess(lines, other_lines):
        median = np.median(np.array([number_of_visible_intersections(l, other_lines, frame) for l in lines]))
        return int(round(median*0.9))

    result = {'left': None, 'right': None, 'top': None, 'bottom': None}
    threshold_for_horizontal_lines = guess(hor, vert)
    result['bottom'] = find_first_line(reversed(hor), lambda h: number_of_visible_intersections(h, vert, frame) >= threshold_for_horizontal_lines)
    result['top'] = find_first_line(hor, lambda h: number_of_visible_intersections(h, vert, frame) >= threshold_for_horizontal_lines)

    threshold_for_vertical_lines = guess(vert, hor)
    result['left'] = find_first_line(vert, lambda v: number_of_visible_intersections(v, hor, frame) >= threshold_for_vertical_lines)
    result['right'] = find_first_line(reversed(vert), lambda v: number_of_visible_intersections(v, hor, frame) >= threshold_for_vertical_lines)

    # estimate x-step for vertical lines
    bottom_line = result["bottom"]
    for v in vert:
        v.x_value = linesIntersection(bottom_line, v)[0]
    x_points = [linesIntersection(bottom_line, v)[0] for v in vert if v.x_value >= result["left"].x_value and v.x_value <= result["right"].x_value]
    distances = [x_points[k] - x_points[k-1] for k in range(1, len(x_points))]
    x_step = np.median(np.array(distances))

    def refine_vertical_border(key):
        direction = +1 if key == "left" else -1
        exact_border_found = False
        current_border = result[key]
        while not exact_border_found:
            next_candidate = nextLine(vert, current_border, direction)
            if next_candidate is None:
                print "No next candidate"
                break
            d = abs(current_border.x_value - next_candidate.x_value) / x_step
            fraction = d - math.floor(d)
            if min(fraction, abs(1-fraction)) < 0.1:
                exact_border_found = True
                result[key] = current_border
            current_border = next_candidate

    refine_vertical_border("left")
    refine_vertical_border("right")

    #-------------------------------------------------------------------------------------
    tempResult = result.copy()
  
    selected_vert = selectLines(vert, tempResult["left"], tempResult["right"])
    selected_hor = selectLines(hor, tempResult["top"], tempResult["bottom"])
    grid = makeGrid(selected_vert, selected_hor)
    
    quality, (hor_m, vert_m), M_to_ideal = matchGridPattern(grid)
    #best, _ = mappings_to_borders(selected_vert, selected_hor, vert_m, hor_m)

    print "bestQuality = ", quality 
    
    
    return None, M_to_ideal, quality
    
###############################################################################################################################
###############################################################################################################################
