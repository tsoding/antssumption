module Ants ( Ants
            , initialAnts
            , updateAnts
            , renderAnts ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Control.Monad
import System.Random
import Debug.Trace

data Role = Soldier | Worker | Gatherer
            deriving (Enum, Show)

data Ant = Ant { antPosition :: Point
               , antRole :: Role
               , antGoal :: Point }

data Ants = Ants { antsAnts :: [Ant]
                 , antsStdGen :: StdGen }

worldSize = 800.0
antSpeed = 100.0

roleColor :: Role -> Color
roleColor Worker = makeColorI 175 239 239 255
roleColor Soldier = makeColorI 239 175 175 255
roleColor Gatherer = makeColorI 175 239 175 255

renderAnt :: Ant -> Picture
renderAnt ant = translate x y
                $ rotate (-heading)
                $ color (roleColor $ antRole ant)
                $ polygon ps
    where ps = [ (-10.0, 10.0)
               , (20.0, 0.0)
               , (-10.0, -10.0)
               ]
          heading = radToDeg $ argV $ (position ! goal)
          position@(x, y) = antPosition ant
          goal = antGoal ant

(!) :: Point -> Point -> Vector
(!) (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

(|+|) :: Vector -> Vector -> Vector
(|+|) (x1, y1) (x2, y2) = (x2 + x1, y2 + y1)

updateAnt :: Float -> StdGen -> Ant -> (Ant, StdGen)
updateAnt deltaTime g ant
    | magV distance < 10.0 =
        let (x, g1) = randomR (-worldSize, worldSize) g
            (y, g2) = randomR (-worldSize, worldSize) g1
        in ( ant { antGoal = (x, y) }
           , g2 )
    | otherwise =
        let heading = argV distance
            position = antPosition ant
        in ( ant { antPosition = position |+| (mulSV (antSpeed * deltaTime) $ unitVectorAtAngle heading) }
           , g )
    where distance = (antPosition ant) ! (antGoal ant)

randomAnt :: IO Ant
randomAnt = do x <- randomRIO (-worldSize, worldSize)
               y <- randomRIO (-worldSize, worldSize)
               role <- toEnum <$> randomRIO (0, 2)
               return $ Ant { antPosition = (x, y)
                            , antRole = role
                            , antGoal = (x, y) }

initialAnts :: Int -> IO Ants
initialAnts n = do ants <- replicateM n randomAnt
                   return $ Ants { antsAnts = ants
                                 , antsStdGen = mkStdGen 42 }

renderAnts :: Ants -> Picture
renderAnts = pictures . map renderAnt . antsAnts

updateAnts :: ViewPort -> Float -> Ants -> Ants
updateAnts _ deltaTime state = let (ants', gp) = foldl (\(ants, g) ant ->
                                                            let (ant', g') = updateAnt deltaTime g ant
                                                            in ((ant':ants), g'))
                                                       ([], antsStdGen state)
                                                       (antsAnts state)
                               in state { antsAnts = ants'
                                        , antsStdGen = gp }
