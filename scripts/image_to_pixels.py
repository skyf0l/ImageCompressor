#!/usr/bin/env python3

from PIL import Image
from sys import argv

if __name__ == '__main__':
    if len(argv) != 2:
        print('Wrong usage: try ./image_to_pixels image.[jpg/png/bmp]')
        exit(1)
    with Image.open(argv[1]) as img:
        pixels = img.load()
        width, height = img.size

        for y in range(height):
            for x in range(width):
                print(f'({y},{x}) ({pixels[x, y][0]},{pixels[x, y][1]},{pixels[x, y][2]})')
