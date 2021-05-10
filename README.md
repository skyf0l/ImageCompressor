# ImageCompressor

ImageCompressor in Haskell based on K-Means algorithm

All automatic tests of Epitech are passed at 100%

## Config

Via stack

```
$ stack build --copy-bins --local-bin-path ./
```

Via Makefile (with stack)

```
$ make
```

## Usage

To compress image: `./compress.py image.[jpg/png/bmp] nbClusters convLimit`

```
$ ./compress.py images/mona-lisa.jpg 10 0.2
Convert image in pixels...
 -> Convert image in pixels done in 0.241s
Compress pixels...
 -> Compress pixels done in 2.52s
Pixels image in image...
 -> Pixels image in image done in 0.563s

-> All done in 3.325s
Output file: images/compressed-mona-lisa.jpg
```

## Example with image of 256 * 256 pixels

### Original

![mona-lisa](.github/readme/mona-lisa.jpg)

### Compressed with 5 clusters and convergence limit at 1

~ 2.5s of compute

![compressed-mona-lisa](.github/readme/compressed_5-clusters_1.0-conv-limit_mona-lisa.jpg)

### Compressed with 10 clusters and convergence limit at 0.5

~ 2.8s of compute

![compressed-mona-lisa](.github/readme/compressed_10-clusters_0.5-conv-limit_mona-lisa.jpg)

### Compressed with 50 clusters and convergence limit at 0.01

~ 3s of compute

![compressed-mona-lisa](.github/readme/compressed_50-clusters_0.01-conv-limit_mona-lisa.jpg)

## Performance

### Original 720p (1280 * 720 pixels)

- Up ot 2Go of ram used for compression

![minecraft](.github/readme/minecraft-1280x720.jpg)

#### Compressed with 16 clusters and convergence limit at 0.5

~ 45s of compute

![compressed-minecraft](.github/readme/compressed_16-clusters_0.5-conv-limit_minecraft-1280x720.jpg)

#### Compressed with 64 clusters and convergence limit at 0.01

~ 60s of compute

![compressed-minecraft](.github/readme/compressed_64-clusters_0.01-conv-limit_minecraft-1280x720.jpg)

#### Compressed with 256 clusters and convergence limit at 0.001

~ 120s of compute

![compressed-minecraft](.github/readme/compressed_256-clusters_0.001-conv-limit_minecraft-1280x720.jpg)

### Original 4K (3840 * 2160 pixels)

- Up ot 13Go of ram used for compression

![mustang](.github/readme/mustang4K.jpg)

#### Compressed with 16 clusters and convergence limit at 0.5

~ 10m of compute

![compressed-mustang](.github/readme/compressed_16-clusters_0.5-conv-limit_mustang4K.jpg)

#### Compressed with 64 clusters and convergence limit at 0.01

~ 12m of compute

![compressed-mustang](.github/readme/compressed_64-clusters_0.01-conv-limit_mustang4K.jpg)

#### Compressed with 256 clusters and convergence limit at 0.001

~ 18m of compute

![compressed-mustang](.github/readme/compressed_256-clusters_0.001-conv-limit_mustang4K.jpg)