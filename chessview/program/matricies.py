import numpy as np
import cv2
import ast
import re
from geometry import lines_intersection

def make_transformation_matricies(border_lines, points=None):
    unitSquare = np.array(points or [[[0, 0],[1, 0],[1, 1],[0, 1]]]).astype("float32")
    border_corners = np.zeros((4, 2), dtype = "float32")

    border_corners[0] = np.array(lines_intersection(border_lines["left"], border_lines["top"]))
    border_corners[1] = np.array(lines_intersection(border_lines["right"], border_lines["top"]))
    border_corners[2] = np.array(lines_intersection(border_lines["right"], border_lines["bottom"]))
    border_corners[3] = np.array(lines_intersection(border_lines["left"], border_lines["bottom"]))

    M = cv2.getPerspectiveTransform(unitSquare, border_corners)
    Minverse = cv2.getPerspectiveTransform(border_corners, unitSquare)  
    return M, Minverse
    
def make_inverse_matrix(matrix):
    if matrix is None: return None
    unit_square = np.array([[[0, 0],[100, 0],[100, 100],[0, 100]]]).astype("float32")
    unit_square_image = cv2.perspectiveTransform(unit_square, matrix)
    inverse = cv2.getPerspectiveTransform(unit_square_image, unit_square) 
    return inverse

###############################################################################################################################

def load_matrices(filename):
    "Load transformation matrices from the log file processed by the following commands."
    # sed '/^locateChessboard/s/ /\n/' botvinnik.output.log >botvinnik.output.log.2
    # sed '/^[ ]*\[/s/\([0-9]\)[ ][ ]*/\1, /g' botvinnik.output.log.2 >botvinnik.output.log.3

    def get_frame_number(s):
        if re.search("FRAME", s) is not None:
            m = re.search("[0-9][0-9]*", s)
            if m is not None:
                return int(s[m.start():m.end()])
        return None

    matrix_line = 0
    matrix_data = ""
    matrices = {}
    
    with open(filename) as logfile:
        for line in logfile:
            frame_number = get_frame_number(line) or frame_number

            if re.search(r"locate", line) is not None:
                matrix_line = 3
                matrix_data = ""
            elif re.search(r"^[ ]*\[", line) is not None:
                if matrix_line > 0:
                    matrix_data = matrix_data + line + ("," if matrix_line >= 2 else "")
                    matrix_line -= 1

                    if matrix_line == 0:
                        try:
                            matrices[frame_number] = np.array(ast.literal_eval(matrix_data))
                        except Exception:
                            print "===> ", matrix_data, line
                            raise
            else:
                matrix_line = 0

    return matrices
    
