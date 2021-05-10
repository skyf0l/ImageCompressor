#!/usr/bin/env python3

from sys import argv
import os
import time

binName = "imageCompressor"

if __name__ == '__main__':
    if not (len(argv) == 4 or len(argv) == 5):
        print(
            'Wrong usage: try ./compress.py image.[jpg/png/bmp] nbClusters convLimit [output_path]')
        exit(1)
    nbClusters = int(argv[2])
    convLimit = float(argv[3])
    imagePath = argv[1]
    imageName = os.path.basename(imagePath)

    if (len(argv) == 4):
        dirName = os.path.dirname(imagePath)
        if len(dirName) != 0:
            dirName += '/'
        outPath = f'{dirName}compressed_{nbClusters}-clusters_{convLimit}-conv-limit_{imageName}'
    else:
        outPath = argv[4]

    if not os.path.isfile(binName):
        print(f'Can not found {binName}\nPlease, do "make"')
        exit(1)

    start = time.time()

    relative_start = time.time()
    print('Convert image in pixels...')
    os.system(f'python3 scripts/image_to_pixels.py {imagePath} > pixels/{imageName}.in')
    print(f' -> Convert image in pixels done in {round((time.time() - relative_start) * 1000) / 1000}s')

    relative_start = time.time()
    print('Compress pixels...')
    os.system(f'./imageCompressor -n {nbClusters} -l {convLimit} -f pixels/{imageName}.in > pixels/{imageName}.out')
    print(f' -> Compress pixels done in {round((time.time() - relative_start) * 1000) / 1000}s')

    relative_start = time.time()
    print('Pixels image in image...')
    os.system(f'python3 scripts/pixels_to_image.py pixels/{imageName}.out {outPath}')
    print(f' -> Pixels image in image done in {round((time.time() - relative_start) * 1000) / 1000}s')

    print()
    print(f'-> All done in {round((time.time() - start) * 1000) / 1000}s')
    print(f'Output file: {outPath}')
