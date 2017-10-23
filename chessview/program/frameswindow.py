from locatechessboard import locateChessboard

class FrameData:
    def __init__(self, number, image):
        self.number = number 
        self.image = image
        self.matrix = None
        self.quality = None
        self.prev_matrix = None
        self.prev_quality = None
        self.next_matrix = None
        self.next_quality = None
        
        self.best_matr = None
        self.best_qual = None

    def set_best(self, matrix, quality):
        if (self.best_qual is None) or (self.best_qual > quality):
            self.best_qual = quality
            self.best_matr = matrix.copy()

    def set_current(self, matrix, quality):
        self.matrix = matrix.copy()
        self.quality = quality
        self.set_best(matrix, quality)
        
    def set_previous(self, matrix, quality):
        self.prev_matrix = matrix.copy()
        self.prev_quality = quality
        self.set_best(matrix, quality)
        
    def set_next(self, matrix, quality):
        self.next_matrix = matrix.copy()
        self.next_quality = quality
        self.set_best(matrix, quality)
        
class FramesWindow:
    def __init__(self, size, callback=lambda x: x):
        self.size = size
        self.queue = [None for i in range(size)]
        self.head = 0
        self.tail = 0
        self.callback = callback

    def previous_index(self, index):
        return (index - 1) % self.size

    def next_index(self, index):
        return (index + 1) % self.size

    def backward_propagation(self, current, matrix):
        index = current
        M = matrix.copy()
        while True:
            index = self.previous_index(index)
            frame = self.queue[index]
            old_qual = frame.best_qual
            _, matrix_to_ib, quality = locateChessboard(frame.image, hint=M)
            print "backward_propagation on frame %s(%s): old_quality = %s --> quality = %s" % (frame.number, index, old_qual, quality)
            frame.set_next(matrix_to_ib, quality)
            M = matrix_to_ib.copy()
            if quality > old_qual or index == self.head:
                break

    def add_frame(self, frame):
        current = self.tail
        prev = self.previous_index(current)

        prevFrame = self.queue[prev]

        if prevFrame is None:
            number = 0
            currFrame = FrameData(number, frame)
            _, matrix_to_ideal_board, quality = locateChessboard(frame)
            currFrame.set_current(matrix_to_ideal_board, quality)

        else:
            number = (prevFrame.number + 1)
            hint = prevFrame.best_matr.copy()

            currFrame = FrameData(number, frame)
    
            _, matrix_to_ideal_board, quality = locateChessboard(frame, hint=hint)
            currFrame.set_previous(matrix_to_ideal_board, quality)

            if quality is not None and quality > 45:
                _, matrix_to_ideal_board, quality = locateChessboard(frame)
                currFrame.set_current(matrix_to_ideal_board, quality)
                self.backward_propagation(current, matrix_to_ideal_board)

        self.queue[current] = currFrame
        self.tail = (self.tail + 1) % self.size

        if self.tail == self.head:
            self.callback(self.queue[self.head])
            self.queue[self.head] = None
            self.head = (self.head + 1) % self.size