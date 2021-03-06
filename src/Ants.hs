module Ants ( Ants
            , initialAnts
            , updateAnts
            , renderAnts ) where

import qualified Foreign as F
import Data.List
import Data.Function
import System.IO.Unsafe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Control.Monad
import System.Random
import Debug.Trace

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Role = Soldier | Worker | Gatherer
            deriving (Enum, Show, Eq, Ord)

data Ant = Ant { antId :: Int
               , antPosition :: Point
               , antRole :: Role
               , antGoal :: Point
               , antRoleStatistacs :: Map.Map Role Int
               , antNearbyAnts :: Set.Set Int
               } deriving (Eq, Ord)

data Ants = Ants { antsAnts :: [Ant]
                 , antsStdGen :: StdGen }

worldSize = 800.0
antSpeed = 100.0
closeEnoughRange = 100.0

roleColor :: Role -> Color
roleColor Worker = makeColorI 100 239 239 255
roleColor Soldier = makeColorI 239 100 100 255
roleColor Gatherer = makeColorI 100 239 100 255

renderAnt :: Ant -> Picture
renderAnt ant = translate x y
                $ rotate (-heading)
                $ pictures [ -- color white $ scale textScale textScale $ text $ show $ Set.size $ antNearbyAnts ant
                            color (roleColor $ antRole ant) $ polygon ps ]
    where ps = [ (-10.0, 10.0)
               , (20.0, 0.0)
               , (-10.0, -10.0)
               ]
          heading = radToDeg $ argV $ (position ! goal)
          position@(x, y) = antPosition ant
          goal = antGoal ant
          textScale = 0.25

initStats :: Map.Map Role Int
initStats = Map.fromList $ zip (map toEnum [0 .. 2]) $ cycle [0]

(!) :: Point -> Point -> Vector
(!) (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

(|+|) :: Vector -> Vector -> Vector
(|+|) (x1, y1) (x2, y2) = (x2 + x1, y2 + y1)

antCloseEnough :: Ant -> Ant -> Bool
antCloseEnough ant1 ant2 =
    magV (antPosition ant1 ! antPosition ant2) <= closeEnoughRange

nearbyAnts :: Ant -> [Ant] -> [Ant]
nearbyAnts ant allAnts =
    filter (antCloseEnough ant)
    $ filter (\x -> antId x /= antId ant) allAnts

updateStats :: Map.Map Role Int -> Set.Set Ant -> Map.Map Role Int
updateStats stats newAnts = foldl (\accStats ant -> Map.insertWith (+) (antRole ant) 1 accStats) stats
                            $ Set.toList newAnts

updateAntStats :: Ant -> Set.Set Ant -> Ant
updateAntStats ant nextNearbyAnts = ant { antNearbyAnts = Set.map antId nextNearbyAnts
                                        , antRoleStatistacs = updateStats (antRoleStatistacs ant) newAnts
                                        }
    where nearbyAntIds = antNearbyAnts ant
          newAnts = Set.filter (\x -> not $ Set.member (antId x) nearbyAntIds) nextNearbyAnts

updateRoleByStats :: Ant -> Ant
updateRoleByStats ant = ant { antRole = nextRole }
    where nextRole = case sortBy (compare `on` snd) $ Map.toList stats of
                       (role, count):_ -> if (n > 0) && (fromIntegral count / fromIntegral n < 0.10)
                                          then role
                                          else antRole ant
                       _ -> antRole ant
          stats = antRoleStatistacs ant
          n = sum $ map snd $ Map.toList stats

switchAntRole :: [Ant] -> Ant -> Ant
switchAntRole ants ant = updateRoleByStats
                         $ updateAntStats ant
                         $ Set.fromList
                         $ nearbyAnts ant ants

switchAntGoal :: Ant -> StdGen -> (StdGen, Ant)
switchAntGoal ant g
    | magV distance < 10.0 =
        let (x, g1) = randomR (-worldSize, worldSize) g
            (y, g2) = randomR (-worldSize, worldSize) g1
        in ( g2, ant { antGoal = (x, y) } )
    | otherwise = (g, ant)
    where distance = (antPosition ant) ! (antGoal ant)

stepAnt :: Float -> Ant -> Ant
stepAnt deltaTime ant = ant { antPosition = position |+| step }
    where position = antPosition ant
          goal = antGoal ant
          heading = argV distance
          distance = position ! goal
          step = mulSV (antSpeed * deltaTime) $ unitVectorAtAngle heading

updateAnt :: Ant -> Float -> StdGen -> [Ant] -> (StdGen, Ant)
updateAnt ant deltaTime g allAnts =
    fmap (switchAntRole allAnts)
    $ fmap (stepAnt deltaTime)
    $ switchAntGoal ant g

randomAnt :: IO Ant
randomAnt = do x <- randomRIO (-worldSize, worldSize)
               y <- randomRIO (-worldSize, worldSize)
               -- role <- toEnum <$> randomRIO (0, 2)
               return $ Ant { antId = 0
                            , antPosition = (x, y)
                            , antRole = Gatherer
                            , antGoal = (x, y)
                            , antRoleStatistacs = initStats
                            , antNearbyAnts = Set.empty
                            }

identifyAnt :: Int -> Ant -> Ant
identifyAnt id ant = ant { antId = id }

initialAnts :: Int -> IO Ants
initialAnts n = do ants <- zipWith identifyAnt [1 .. n] <$> replicateM n randomAnt
                   return $ Ants { antsAnts = ants
                                 , antsStdGen = mkStdGen 42 }

renderAnts :: Ants -> Picture
renderAnts = pictures . map renderAnt . antsAnts

updateAnts :: ViewPort -> Float -> Ants -> Ants
updateAnts _ deltaTime state = let (gp, ants') = foldl (\(g, ants) ant ->
                                                            let (g', ant') = updateAnt ant deltaTime g (antsAnts state)
                                                            in (g', (ant':ants)))
                                                       (antsStdGen state, [])
                                                       (antsAnts state)
                               in state { antsAnts = ants'
                                        , antsStdGen = gp }
