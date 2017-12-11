module Main where

import Graphics.Gloss
import Ants

main :: IO ()
main = simulate (InWindow "Antssumption" (800, 600) (10, 10))
                (makeColorI 33 33 33 255)
                30
                Ants
                renderAnts
                updateAnts
