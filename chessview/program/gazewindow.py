import json
import numpy as np
import cv2
import math

class GazeWindow:
    def __init__(self, filename=None):
        self.data = {}

