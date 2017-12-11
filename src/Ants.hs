module Ants ( Ants
            , initialAnts
            , updateAnts
            , renderAnts ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Ant = Ant { antPosition :: Point
               , antHeading :: Float }

data Ants = Ants { antsAnts :: [Ant] }

antColor = makeColorI 175 239 239 255

renderAnt :: Ant -> Picture
renderAnt ant = translate x y
                $ rotate (-heading)
                $ color antColor
                $ polygon ps
    where ps = [ (-10.0, 10.0)
               , (20.0, 0.0)
               , (-10.0, -10.0)
               ]
          heading = antHeading ant
          (x, y) = antPosition ant



updateAnt :: Float -> Ant -> Ant
updateAnt deltaTime ant =
    ant { antHeading = antHeading ant + 45.0 * deltaTime }

initialAnts :: IO Ants
initialAnts = return $ Ants {
                antsAnts = [Ant { antPosition = (0.0, 0.0)
                                , antHeading = 0.0 }]
              }

renderAnts :: Ants -> Picture
renderAnts = pictures . map renderAnt . antsAnts

updateAnts :: ViewPort -> Float -> Ants -> Ants
updateAnts _ deltaTime state = state { antsAnts = map (updateAnt deltaTime)
                                                  $ antsAnts state }
