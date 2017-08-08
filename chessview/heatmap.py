import numpy as np
import argparse
import glob
import cv2
import scipy
import scipy.spatial, scipy.spatial.distance
import scipy.cluster.hierarchy
import math


files = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
ranks = ['1', '2', '3', '4', '5', '6', '7', '8']
ranks.reverse()

def readFile(filename):
    gaze_squares = []
    view_log = open(filename)
    for line in view_log:
        frame_number, frame_ms, square = line.split()
        frame_number = int(frame_number)
        frame_ms = float(frame_ms)
        gaze_squares.append(viewPoint(frame_ms, square))
        
        #print frame_ms, square#, gaze_squares
    view_log.close()
    return gaze_squares
        
class viewPoint:
    def __init__(self, time, square):
        self.time = time
        self.square = square
        self.file = "abcdefgh".index(square[0]) 
        self.rank = int(square[1])
        #print self.file, self.rank
        
def makeDataToPlot(gaze_squares):
    data = np.zeros((8, 8), dtype = "float")
    norm = len(gaze_squares)
    add = 1.0/norm
    for sq in gaze_squares:
        if sq.rank in range(8) and sq.file in range(8):
            data[7-sq.rank][sq.file] += 1.0 #add
    return data
    
def allow_to_add(c, n):
    return (c.square != n.square) and (abs(n.time - c.time) < 100.0)    
    
def arrowData(gaze_squares):
    data = np.zeros((8, 8), dtype = "float")
    sqtosq = {}
    norm = len(gaze_squares)
    
    cchain = []
    
    traj = []
    traj.append(viewPoint(0, 'd3'))
    traj.append(viewPoint(0, 'e3'))
    traj.append(viewPoint(0, 'f3'))
    traj.append(viewPoint(0, 'g3'))
    
    for i in range(norm - 1):
        current = gaze_squares[i]
        next = gaze_squares[i+1]
        cur_square = gaze_squares[i].square
        next_square = gaze_squares[i+1].square   
             
        time = next.time - current.time
        if cur_square == next_square: # or time > 100.0:
            continue
            print cur_square, '->', next_square, time
            
        if cchain == []:
            cchain.append(current)
            
        if allow_to_add(current, next):
            cchain.append(next)
        else:
            print 'arrowData:cchain ', [c.square for c in cchain]
            ind = findTrajInCchain(traj, cchain)
            if ind is not None: print ind
            cchain = [next]
            
        if sqtosq.get((cur_square, next_square)) is not None:
            sqtosq[(cur_square, next_square)] += 1
        else:
            sqtosq[(cur_square, next_square)] = 1
    return sqtosq
        
#def findTemplate(template, gaze_squares):


#def metaSquare(sq):
#    return []

def inNeighborhood(sq, c):
    return (abs(sq.file - c.file) <= 1) and (abs(sq.rank - c.rank) <= 1)

    
def findTrajInCchain(traj, cchain, ccoffset=0):
    if len(traj) == 0 or len(traj) > len(cchain):
        return []
        
    sq = traj[0]
    ccoff=ccoffset
    flag = 0
    matches = []
    for c_index, c in enumerate(cchain[ccoffset:]):
        if inNeighborhood(sq, c):
            ccoff += c_index + 1
            tails = findTrajInCchain(traj[1:], cchain, ccoffset=ccoff)
            if tails is not None:
                flag = 1 
                matches.extend([[ccoff]+tail for tail in tails] if tails else [[ccoff]])
    if flag==0: 
        return None
    return matches
            
        
#gaze_squares = readFile('view_log.log')
gaze_squares = readFile('botvinnik.log')
#-----------------------------------------------------------------------------------------
import numpy as np
import matplotlib.pyplot as plt

fig, ax = plt.subplots()

min_val, max_val, diff = 0., 8., 1.

imshow_data = makeDataToPlot(gaze_squares)
arrow_data = arrowData(gaze_squares)

print arrow_data

im = ax.imshow(imshow_data, interpolation='nearest')

ax.set_xticks(np.arange(min_val-diff/2, max_val-diff/2))
ax.set_yticks(np.arange(min_val-diff/2, max_val-diff/2))
ax.tick_params(direction='inout', pad=5, labelbottom='on', labeltop='on', labelleft='on', labelright='on')
ax.set_xticklabels(files, ha='left')
ax.set_yticklabels(ranks)


dx = np.linspace(0,1,8)
X,Y = np.meshgrid(dx,dx)
Z  = X**2 - Y
COLOR = X

def discrete_cmap(N, base_cmap=None):
    """Create an N-bin discrete colormap from the specified input map"""

# Note that if base_cmap is a string or None, you can simply do
    #    return plt.cm.get_cmap(base_cmap, N)
    # The following works for string, None, or a colormap instance:

    base = plt.cm.get_cmap(base_cmap)
    color_list = base(np.linspace(0, 1, N))
    cmap_name = base.name + str(N)
    return base.from_list(cmap_name, color_list, N)



#plt.imshow(Z)
#plt.colorbar()

#plt.quiver(X,Y,COLOR,width=.01,linewidth=1)
#plt.colorbar() 

#dx = np.linspace(0,1,6)
#X,Y = np.meshgrid(dx,dx)
#plt.quiver(X,Y,COLOR,width=.01,linewidth=1)

#plt.show(im)

fig.subplots_adjust(right=0.8)
cbar_ax = fig.add_axes([0.85, 0.15, 0.05, 0.7])
fig.colorbar(im, cax=cbar_ax)

fig1 = plt.gcf()
#plt.show()
fig1.savefig('watermark.png', format="png",  dpi=100)

from PIL import Image

text_img = Image.new('RGBA', (800,600), (0, 0, 0, 0))
watermark = Image.open('watermark.png')
img = Image.open('Untitled.png')
    
r, g, b, a = img.split()
top = Image.merge("RGB", (r, g, b))
a = r.point(lambda x: 120)
mask = Image.merge("L", (a,))

text_img.paste(watermark, (0,0))
text_img.paste(top, (128,58), mask=mask)
text_img.save("img_with_watermark.png", format="png")


#watermark.paste(img, (0, 0), img)
#watermark.save("img_with_watermark.png")



#-----------------------------------------------------------------------------------------

#def makeMap(points):
    
        
    
