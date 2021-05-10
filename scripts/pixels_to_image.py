#!/usr/bin/env python3

from PIL import Image
from sys import argv
import numpy as np
import re

if __name__ == '__main__':
    if not (len(argv) == 2 or len(argv) == 3):
        print('Wrong usage: try ./image_to_pixels image [savePath]')
        exit(1)
    with open(argv[1]) as f:

        color = None
        pixels = {}
        width = 0
        height = 0
        for line in f:
            line = line.strip()
            if '--' in line:
                strColor = f.readline().strip()
                color = list(map(int, re.findall(r'[0-9]+', strColor)))
                f.readline()
                continue
            if color is None:
                print('Wrong format')
                exit(1)
            pos = tuple(map(int, re.findall(r'[0-9]+', line.split(' ')[0])))
            baseColor = tuple(map(int, re.findall(r'[0-9]+', line.split(' ')[1])))
            x, y = pos
            pixels[x, y] = color
            if x > width:
                width = x
            if y > height:
                height = y

        width += 1
        height += 1
        imgData = np.zeros((width, height, 3), dtype=np.uint8)

        for x, y in pixels:
            imgData[x, y] = pixels[x, y]

        image = Image.fromarray(imgData)
        if len(argv) == 2:
            image.show()
        else:
            image.save(argv[2])
