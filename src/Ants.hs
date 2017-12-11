module Ants where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Ants = Ants

renderAnts :: Ants -> Picture
renderAnts _ = Blank

updateAnts :: ViewPort -> Float -> Ants -> Ants
updateAnts _ _ ants = ants
