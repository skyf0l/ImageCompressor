module Usage
  ( usage,
  )
where

usage :: IO ()
usage =
  putStr "USAGE: ./imageCompressor -n N -l L -f F\n\n"
    >> putStr "\tN\tnumber of colors in the final image\n"
    >> putStr "\tL\tconvergence limit\n"
    >> putStr "\tF\tpath to the file containing the colors of "
    >> putStr "the pixels\n"