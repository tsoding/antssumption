module Ants ( Ants
            , initialAnts
            , updateAnts
            , renderAnts ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Control.Monad
import System.Random

data Ant = Ant { antPosition :: Point
               , antHeading :: Float }

data Ants = Ants { antsAnts :: [Ant] }

antColor = makeColorI 175 239 239 255
worldSize = 800.0

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
    ant { antHeading = antHeading ant + 180 * deltaTime }

randomAnt :: IO Ant
randomAnt = do x <- randomRIO (-worldSize, worldSize)
               y <- randomRIO (-worldSize, worldSize)
               heading <- randomRIO (0.0, 360.0)
               return $ Ant { antPosition = (x, y)
                            , antHeading = heading }

initialAnts :: Int -> IO Ants
initialAnts n = do ants <- replicateM n randomAnt
                   return $ Ants { antsAnts = ants } -- ants

renderAnts :: Ants -> Picture
renderAnts = pictures . map renderAnt . antsAnts

updateAnts :: ViewPort -> Float -> Ants -> Ants
updateAnts _ deltaTime state = state { antsAnts = map (updateAnt deltaTime)
                                                  $ antsAnts state }
