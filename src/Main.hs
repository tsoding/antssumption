module Main where

import Graphics.Gloss
import Ants

main :: IO ()
main = do ants <- initialAnts 1000
          simulate (InWindow "Antssumption" (800, 600) (10, 10))
                   (makeColorI 33 33 33 255)
                   30
                   ants
                   renderAnts
                   updateAnts
