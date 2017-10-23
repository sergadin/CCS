import json
import numpy as np
import cv2
import math

from collections import deque

import gvars as gvars
from matricies import make_inverse_matrix
from painting import paint_ideal_board

class GazeStream:
    def __init__(self, filename=None, history_size=2000):
        self.last_line = None
        self.calls = None
        self.history = deque([], history_size)
        self.last_call_extender_ms = 0
        self.keys = []
        if filename is not None:
            self.openStream(filename)

    def openStream(self, filename, keys=['gp', 'gp3', 'gy', 'ac']):
        self.f = open(filename)
        self.keys = keys
        data = {}
        while 'ts' not in data:
            line = self.f.readline()
            data = json.loads(line)
        self.first_ts_microsec = data.get('ts', None)
        data['ms'] = self.convert_to_stream_ms(data['ts'])
        self.f.seek(0)
        self.history.append(data)
        return True

    def convert_to_stream_ms(self, ts):
        return (ts - self.first_ts_microsec) / 1000.0

    def extendDequeByMS(self, extension_ms, delta_ms=100):
        last_ms = self.last_call_extender_ms
        #last_ms = self.convert_to_stream_ms(self.history[-1].get('ts'))

        while True:
            line = self.f.readline()
            if not line:
                break

            data = json.loads(line)
            data['ms'] = self.convert_to_stream_ms(data['ts'])
            self.history.append(data)

            data_ms = self.convert_to_stream_ms(data.get('ts'))
            if data_ms > last_ms + extension_ms + delta_ms:
                break
        self.last_call_extender_ms += extension_ms

    def getDataFromPeriod(self, start_ms, end_ms):
        if self.last_call_extender_ms < start_ms:
            self.extendDequeByMS(start_ms - self.last_call_extender_ms)

        data_dict = dict((el, []) for el in self.keys)
        data_dict[None] = []

        def get_key(d):
            for key in self.keys:
                if d.has_key(key):
                    return key
            return None
            #raise Exception("No valid key")

        def correct_data(d):
            return (d.get('s', 1) == 0)

        if self.last_call_extender_ms < end_ms:
            self.extendDequeByMS(end_ms - self.last_call_extender_ms)

        for d in self.history:
            if start_ms <= d['ms'] and d['ms'] < end_ms and correct_data(d):
                data_dict[get_key(d)].append(d)

        return data_dict

    # def nextData(self, offset_ms=None, key='gp', epsilon_ms=25):
    #     data = {}
    #     last_call = self.calls.get(key)
    #
    #     while True:
    #         if self.last_line is not None:
    #             line = self.last_line
    #             self.last_line = None
    #         else:
    #             line = self.f.readline()
    #         if not line:
    #             return (None, None)
    #         data = json.loads(line)
    #         data_ts = data.get('ts') or offset_ms
    #         if key not in data:
    #             continue
    #         stream_offset_ms = (data_ts - self.first_ts_microsec) / 1000.0
    #         if offset_ms is None or abs(stream_offset_ms - offset_ms) <= epsilon_ms:
    #             break
    #         if offset_ms is not None and stream_offset_ms > offset_ms:
    #             self.last_line = line
    #             break
    #
    #     if data_ts is not None:
    #         self.calls[key] = data_ts
    #
    #     return (stream_offset_ms, data.get(key))

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
    #print ideal_gaze_point
    
    #paint_ideal_board(make_inverse_matrix(frame_to_ideal_matrix), color=[10, 100, 230]) #yellow_points
    #cv2.circle(gvars.image_to_draw_121, (int(x), int(y)), radius=8, color=[10, 100, 230], thickness=2)
    
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
 

