import json
import numpy as np
import cv2

import gvars as gvars
from matricies import make_inverse_matrix
from painting import paint_ideal_board


class GazeStream:
    def __init__(self, filename=None):
        self.last_line = None
        if filename is not None:
            self.openStream(filename)

    def openStream(self, filename):
        self.f = open(filename)
        data = {}
        while 'ts' not in data:
            line = self.f.readline()
            data = json.loads(line)
        self.first_ts_microsec = data.get('ts', None)
        if 'gp' in data:
            self.f.seek(0)
        return True

    def nextGaze(self, offset_ms=None, epsilon_ms=25):
        data = {}
        while True:
            if self.last_line is not None:
                line = self.last_line
                self.last_line = None
            else:
                line = self.f.readline()
            if not line:
                return (None, None)
            data = json.loads(line)
            if 'gp' not in data:
                continue
            stream_offset_ms = (data.get('ts') - self.first_ts_microsec)/1000.0
            if offset_ms is None or abs(stream_offset_ms - offset_ms) <= epsilon_ms:
                break
            if offset_ms is not None and stream_offset_ms > offset_ms:
                self.last_line = line
                break
        return (stream_offset_ms, data.get('gp'))

    def close(self):
        self.f.close()

def find_gaze_square(normalizedGazePoint, frameSize, frame_to_ideal_matrix):
    gx, gy = normalizedGazePoint
    width, height = frameSize

    x, y = gx*width, gy*height
    ideal_gaze_point = cv2.perspectiveTransform(np.array([[[x, y]]]).astype("float32"), frame_to_ideal_matrix)[0][0]
    print ideal_gaze_point
    
    paint_ideal_board(make_inverse_matrix(frame_to_ideal_matrix), color=[10, 100, 230]) #yellow_points
    cv2.circle(gvars.image_to_draw_121, (int(x), int(y)), radius=8, color=[10,100,230], thickness=2)
    
    igx = ideal_gaze_point[0]
    igy = ideal_gaze_point[1]
    print igx, igy
    
    if igx < 0 or igx > 1 or igy < 0 or igy > 1:
        return None

    files = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
    ranks = ['1', '2', '3', '4', '5', '6', '7', '8']
    ranks.reverse()
    ideal_square_width = 1.0/8

    return files[int(math.floor(igx/ideal_square_width))] + ranks[int(math.floor(igy/ideal_square_width))]
 

