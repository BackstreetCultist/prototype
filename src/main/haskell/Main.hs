{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import System.Random
import System.IO.Unsafe
import Data.List
import Data.Ord
import Control.Monad
import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)

-- Define some useful data types
type Heuristic = [Char]
type SolutionObjectiveValue = Int
type Solution = (Heuristic, SolutionObjectiveValue)
type Population = [Solution]

-- impure function
foreign import ccall "JavaCPP_init" c_javacpp_init :: CInt -> Ptr (Ptr CString) -> IO ()
javacpp_init :: IO ()
javacpp_init = c_javacpp_init 0 nullPtr

-- pure function
foreign import ccall "cppExposedHeuristic" c_cppExposedHeuristic :: CString -> CInt
cppExposedHeuristic :: Heuristic -> SolutionObjectiveValue
cppExposedHeuristic h = fromIntegral (c_cppExposedHeuristic (unsafePerformIO(newCString h)))
--remember to free this C string afterwards

--Generate an initial population of heuristics
generateHeuristicSet :: Population
generateHeuristicSet = [(generateHeuristic (mkStdGen 1), 0)] ++ [(generateHeuristic (mkStdGen 2), 0)] ++ [(generateHeuristic (mkStdGen 3), 0)] ++ [(generateHeuristic (mkStdGen 4), 0)]

--Helper function for the above - generate a single new heuristic
generateHeuristic :: StdGen -> Heuristic
generateHeuristic gen = do
    take 8  (randomRs ('0', '1') gen)

--Run a heuristic in Java, and get the resulting SOVs
runHeuristic :: Heuristic -> Solution
runHeuristic h = (h, cppExposedHeuristic h)

--Test a set of heuristics, and store their SOVs
testPopulation :: Population -> Population
testPopulation = map (runHeuristic . fst)
-- testPopulation xs = map runHeuristic (map fst xs)

--Choose two parents from the current population
selectParents :: Population -> (Heuristic, Heuristic)
selectParents xs = (fst(ys !! 2), fst(ys !! 3))
                    where
                      ys = sortBy (comparing snd) xs
--Naive solution - choose two best

-- --From two parents, create two new child heuristics
-- evolveHeuristics :: (Heuristic, Heuristic) -> (Heuristic, Heuristic)
-- --TODO implement

-- --Choose a heuristic from the current population to undergo mutation
-- selectMutateHeuristic :: Population -> Heuristic
-- --TODO implement

-- --Mutate a heuristic
-- mutateHeuristic :: Heuristic -> Heuristic
-- --TODO implement


-- mutateHeuristic :: Heuristic -> Heuristic
-- mutateHeuristic h = a ++ b ++ c
--                     where
--                       (a, c) = splitAt x h
--                       x = getRandomIndex h
-- note - use Objective Value as seed?

-- getRandomIndex :: [a] -> Int
-- getRandomIndex xs = 

-- --Bit Flipper helper function to mutate heuristics
-- flipBit :: Char -> Char
-- flipBit '0' = '1'
-- flipBit '1' = '0'
-- flipBit _ = '0' --TODO do I need this?

main :: IO ()
main = do
  javacpp_init
  print $ cppExposedHeuristic "Hello"
  print $ generateHeuristicSet
