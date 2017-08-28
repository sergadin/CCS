import numpy as np
import cv2
import gvars as gvars
from matricies import make_inverse_matrix
    
def draw_line(image, line, color=(255, 255, 255), width=1):
    cv2.line(image, (line.x1, line.y1), (line.x2, line.y2), color, width)

def rescale(img, factor=0.7):
    res = cv2.resize(img, None, fx=factor, fy=factor, interpolation = cv2.INTER_CUBIC)
    return res

def painting(image, corners, centroids, vert, hor):
    height, width, point = image.shape
    for line in vert:
        [x1,y1,x2,y2] = line.as_points()
        cv2.line(image,(x1,y1),(x2,y2),(0,0,255),1)
    for line in hor:
        [x1,y1,x2,y2] = line.as_points()
        cv2.line(image,(x1,y1),(x2,y2),(0,255,0),1)
    for [j, i] in corners:
        image[i, j]=[255,0,0]
    for [j, i] in centroids:
        cv2.circle(image, (int(j), int(i)), radius=5, color=[255,200,190], thickness=2)
    
    cv2.line(image,(0,0),(width,height),(23,123,200),2)
    return image

###############################################################################################################################
###############################################################################################################################

def debug_painting(frame, hor, vert, title='locateChessboard'):
    image = frame.copy()
    for line in hor + vert:
        draw_line(image, line, color=(120, 120, 120))
    cv2_imshow_rescaled(title, image)


def paint_array_points(array, color=[55, 200, 190], radius=5, image=None):
    n, m, _ = array.shape
    if image is None:
        image = gvars.image_to_draw_121 #.copy()
    for i in range(n):    
        for j in range(m):
            x, y =  array[i][j]
            cv2.circle(image, (x, y), radius=radius, color=color, thickness=2)
    #cv2.imshow('paint_array_points Total on %s' % frame_number, image)
    #cv2.waitKey(0)

###############################################################################################################################
###############################################################################################################################

def cv2_imshow_rescaled(title, image, scale=0.7):
    scaled_frame = rescale(image, scale)
    cv2.imshow(title, scaled_frame)
    cv2.moveWindow(title, 10, 10)
    cv2.waitKey(0)

def display_hint(frame, vert, hor, hint, vert_m=None, hor_m=None):
    from program.grid import generateIdealBoard
    
    ideal_to_frame = make_inverse_matrix(hint)
    frame_to_ideal = hint
    
    ideal_board = generateIdealBoard()
    ideal_board_on_frame = cv2.perspectiveTransform(ideal_board, ideal_to_frame)

    image = frame.copy()
    for (row_index, line) in enumerate(hor):
        if hor_m is not None and row_index in hor_m:
            draw_line(image, hor[row_index], color=(255, 120, 120), width=2)
        else:
            draw_line(image, hor[row_index], color=(120, 120, 120))
    for (col_index, line) in enumerate(vert):
        if vert_m is not None and col_index in vert_m:
             draw_line(image, vert[col_index], color=(120, 255, 120), width=2)
        else:
            draw_line(image, vert[col_index], color=(120, 120, 120))

    num_lines = 9
    ideal_board_on_prev_frame = cv2.perspectiveTransform(ideal_board, ideal_to_frame)
    for row in range(num_lines):
        for col in range(num_lines):
            x = ideal_board_on_frame[row][col][0]
            y = ideal_board_on_frame[row][col][1]
            cv2.circle(image, (x, y), radius=5, color=[255,200,190], thickness=2)

            x_prev = ideal_board_on_prev_frame[row][col][0]
            y_prev = ideal_board_on_prev_frame[row][col][1]
            cv2.circle(image, (x_prev, y_prev), radius=5, color=[255,100,190], thickness=2)

    cv2_imshow_rescaled('Hinted on %s' % gvars.frame_number, image)


def paint_ideal_board(matrix_to_frame, image=None, color=[150, 100, 100]):
    from program.grid import generateIdealBoard
    ideal_board = generateIdealBoard()
    real_board = cv2.perspectiveTransform(np.array(ideal_board), matrix_to_frame)
    paint_array_points(real_board, color=color, image=image)


