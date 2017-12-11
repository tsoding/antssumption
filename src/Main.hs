module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Ants = Ants

renderAnts :: Ants -> Picture
renderAnts _ = Blank

updateAnts :: ViewPort -> Float -> Ants -> Ants
updateAnts _ _ ants = ants

main :: IO ()
main = simulate (InWindow "Antssumption" (800, 600) (10, 10))
                (makeColorI 33 33 33 255)
                30
                Ants
                renderAnts
                updateAnts
