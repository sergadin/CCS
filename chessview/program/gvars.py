def init():
    global image_to_draw_121
    global image_prev
    global image_with_original_colors
    global frame_number

    image_to_draw_121 = None
    image_prev = None
    image_with_original_colors = None
    frame_number = None

    colors = {
        'green':      (120, 255, 120), # vert_lines
        'king blue':  (255, 120, 120),
        'grey':       (120, 120, 120)
    }