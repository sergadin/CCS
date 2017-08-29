# import the necessary packages
import numpy as np
import argparse
import glob
import cv2
import re
from operator import itemgetter

from program.frameswindow import FramesWindow
from program.gaze import find_gaze_square, GazeStream
from program.matricies import load_matrices, make_inverse_matrix
from program.painting import paint_ideal_board, rescale, cv2_imshow_rescaled
from program.grid import generateIdealBoard
import program.gvars as gvars

gvars.init()

#cap = cv2.VideoCapture("Rec040.mp4")

videowriter = None

cap = cv2.VideoCapture("out.mp4")
gStream = GazeStream("livedata (out).json")

first_frame = 10
last_frame = 1001 # None for infinity
scale = 1.0
scale_to_draw = 0.7
step = 1
use_log = False

dict_frame_to_ms = {}
proc_num = 0

#func to callback from FramesWindow
called_on_frame_ready = 0

def on_frame_ready(frame_data):
    global called_on_frame_ready, videowriter
    frame = frame_data.image
    frame_number = frame_data.number
    frame_rows, frame_cols, _ = frame.shape
    to_ideal_matrix = frame_data.best_matr 
    
    called_on_frame_ready += 1
    ms = dict_frame_to_ms[called_on_frame_ready]
    (offset_ms, gaze_point) = gStream.nextGaze(ms)
    if gaze_point is None:
        print "on_frame_ready", frame_number, str(ms), "---"
    else:    
        gaze_at_square = find_gaze_square(gaze_point, (frame_cols, frame_rows), to_ideal_matrix)
        print "on_frame_ready", frame_number, str(ms), gaze_at_square
        (gx, gy) = gaze_point
        x, y = gx*frame_cols, gy*frame_rows


        cv2.circle(frame, (int(x), int(y)), radius=5, color=[100,10,230], thickness=2)
        paint_ideal_board(make_inverse_matrix(to_ideal_matrix), frame, color=(0, 255, 0))
        if videowriter is not None:
            videowriter.write(frame)

        cv2_imshow_rescaled('on_frame_ready %s' % frame_number, frame, scale=scale_to_draw)

        #cv2.imshow('on_frame_ready %s' % frame_number, image_to_draw_121)
        #cv2.waitKey(0)

        if view_log and gaze_at_square is not None:
            view_log.write(str(frame_number) + ' ' + str(ms) + ' ' + str(gaze_at_square) + '\n')
        
frames_window = FramesWindow(5, on_frame_ready)

matrices = None
if use_log:
    matrices = load_matrices("botvinnik.output.log.3")

view_log = open('view_log_1.log', 'w')

while True:
    flag, frame = cap.read()
    frame_timestamp_ms = cap.get(cv2.cv.CV_CAP_PROP_POS_MSEC)
    gvars.frame_number = cap.get(cv2.cv.CV_CAP_PROP_POS_FRAMES)
    if flag:
        if (gvars.frame_number < first_frame) or ((int(gvars.frame_number)%step) != (int(first_frame)%step)):
            continue
        # The frame is ready and already captured
        print "=========> FRAME ", gvars.frame_number, " <======================="
        
        proc_num += 1
        dict_frame_to_ms[proc_num] = frame_timestamp_ms
                
        if scale < 1.0:
            frame = rescale(frame, scale)

        frame_rows, frame_cols, _ = frame.shape

        if videowriter is None:
            #videowriter = cv2.VideoWriter(filename='results.mp4', fourcc=cv2.CV_FOURCC('X','V','I','D'), fps=25, frameSize=(frame_rows, frame_cols))
            fourcc = cv2.cv.CV_FOURCC(*'XVID')
            videowriter = cv2.VideoWriter('output.avi', fourcc, 25.0, (frame_cols, frame_rows))

        #M = cv2.getRotationMatrix2D((frame_cols/2,frame_rows/2),15,1)
        #frame = cv2.warpAffine(frame,M,(frame_cols,frame_rows))
        gvars.image_to_draw_121 = frame.copy()
        image_to_draw_matrices = frame.copy()
        gvars.image_with_original_colors = frame.copy()

        if use_log and matrices.get(proc_num, None) is not None:
            paint_ideal_board(make_inverse_matrix(matrices[proc_num]), image_to_draw_matrices)
            #cv2_imshow_rescaled('matrices Total on %s' % gvars.frame_number, image_to_draw_matrices, scale=scale_to_draw)
            #continue
                        
        frames_window.add_frame(frame)

        #cv2_imshow_rescaled('image_to_draw after all on %s' % gvars.frame_number, gvars.image_to_draw_121, scale=scale_to_draw)

        #image_prev = gvars.image_to_draw_121.copy()
        
    else:
        # The next frame is not ready, so we try to read it again
        #cap.set(cv2.cv.CV_CAP_PROP_POS_FRAMES, pos_frame-1)
        print "frame is not ready"
        # It is better to wait for a while for the next frame to be ready
        cv2.waitKey(1000)

    if cap.get(cv2.cv.CV_CAP_PROP_POS_FRAMES) == cap.get(cv2.cv.CV_CAP_PROP_FRAME_COUNT):
        # If the number of captured frames is equal to the total number of frames,
        # we stop
        break

    if last_frame is not None and cap.get(cv2.cv.CV_CAP_PROP_POS_FRAMES) >= last_frame:
        break
        
view_log.close()
exit(0)

