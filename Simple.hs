module Simple where

import qualified Data.Set as Set

import Debug.Trace

main :: IO ()
main = pure ()

data Tick = Tick
  { height :: Double
  , position :: Double
  , label :: String
  }

render :: Tick -> String
render _ = undefined

type TickGroup = [Tick]

minimumDistance :: TickGroup -> Double
minimumDistance group =
  let orderedPositions = Set.toList $ Set.fromList $ map position group
      differences = map abs $ zipWith (-) orderedPositions (tail orderedPositions)
  in
  minimum differences

type Range = (Double, Double)

divideRange :: [Double] -> Range -> [Range]
divideRange qtys (start, end) =
  let increment = (end - start) / sum qtys
      points = map (start +) $ map (increment *) $ scanl (+) 0 qtys
  in
  zip points (tail points)

takeAmounts :: [Int] -> [a] -> [[a]]
takeAmounts [] _ = []
takeAmounts (i:is) xs =
  let (start, end) = splitAt i xs
  in
  start : takeAmounts is end
