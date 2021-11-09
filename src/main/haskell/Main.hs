{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import System.Random
import System.IO.Unsafe
import Data.List
import Data.Ord
import Data.Map
import Control.Monad
import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)



--Define some useful data types
type HeuristicRepresentation = [Char]
type HeuristicScore = Int
type Heuristic = (HeuristicRepresentation, HeuristicScore)
type HeuristicPopulation = [Heuristic]

type SolutionRepresentation = [Char]
type SolutionObjectiveValue = Int
type Solution = (SolutionRepresentation, SolutionObjectiveValue)
type SolutionPopulation = [Solution]

-- Deprecated - define some useful data types
type TheTypeFormerlyKnownAsSolution = (HeuristicRepresentation, SolutionObjectiveValue)
type TheTypeFormerlyKnownAsPopulation = [TheTypeFormerlyKnownAsSolution]


-- ------------------------------JAVA INTERFACE --------------------------------------------------------- --
-- impure function
foreign import ccall "JavaCPP_init" c_javacpp_init :: CInt -> Ptr (Ptr CString) -> IO ()
javacpp_init :: IO ()
javacpp_init = c_javacpp_init 0 nullPtr

-- pure function
foreign import ccall "cppExposedHeuristic" c_cppExposedHeuristic :: CString -> CString -> CString
cppExposedHeuristic :: HeuristicRepresentation -> SolutionRepresentation -> String
cppExposedHeuristic h s = unsafePerformIO(peekCAString (c_cppExposedHeuristic (unsafePerformIO(newCString h)) (unsafePerformIO(newCString s))))
--remember to free this C string afterwards


-- ------------------------------HELPER FUNCTIONS ------------------------------------------------------- --
--These helper functions will be very useful
getAllHeuristicRepresentations :: HeuristicPopulation -> [HeuristicRepresentation]
getAllHeuristicRepresentations hs = Data.List.map fst hs

getAllSolutionRepresentations :: SolutionPopulation -> [SolutionRepresentation]
getAllSolutionRepresentations sols = Data.List.map fst sols

--Gets a random index in a list
getRandomIndex :: StdGen -> [a] -> Int
getRandomIndex g xs = fst (uniformR (0, length xs - 1) g)

--This helper function to shuffle a list is from https://wiki.haskell.org/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Data.Map.insert j x . Data.Map.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ Data.List.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)


-- ------------------------------INITIAL GENERATION FUNCTIONS ------------------------------------------- --
--Generate an initial population of heuristics with scores of 0
generateHeuristicPopulation :: HeuristicPopulation
generateHeuristicPopulation = [(generateEightBitString (mkStdGen 1), 0)] ++ [(generateEightBitString (mkStdGen 2), 0)] ++ [(generateEightBitString (mkStdGen 3), 0)] ++ [(generateEightBitString (mkStdGen 4), 0)]

--Generate an initial population of solutions with OVs of 0
generateSolutionPopulation :: SolutionPopulation
generateSolutionPopulation = [(generateEightBitString (mkStdGen 5), 0)] ++ [(generateEightBitString (mkStdGen 6), 0)] ++ [(generateEightBitString (mkStdGen 7), 0)] ++ [(generateEightBitString (mkStdGen 8), 0)]

--Helper function for the above - generate a single new heuristic
generateEightBitString :: StdGen -> [Char]
generateEightBitString gen = do
    Data.List.take 8  (randomRs ('0', '1') gen)


-- ------------------------------APPLYING HEURISTICS ---------------------------------------------------- --
--Apply a set of heuristics to a set of solutions
--Score heuristics based on performance
applyHeuristicPopulation :: (HeuristicPopulation, SolutionPopulation) -> (HeuristicPopulation, SolutionPopulation)
applyHeuristicPopulation (hs, sols) = unzip (zipWith (curry applyHeuristic) hsShuffled sols)
                                  where
                                    hsShuffled = fst (fisherYates (mkStdGen (snd(head sols))) hs) --Shuffle list based on one of the OVs

--Apply a heuristic and record its score
applyHeuristic :: (Heuristic, Solution)-> (Heuristic, Solution)
applyHeuristic (h,s) = (h', s')
                    where
                      h' = scoreHeuristic h s s'
                      s' = runHeuristic (fst h) (fst s)

--Run a heuristic on a solution in Java, and get the resulting Solution
runHeuristic :: HeuristicRepresentation -> SolutionRepresentation -> Solution
runHeuristic h s = (representation, value)
                  where
                    representation = Data.List.take 8 result
                    value = read (Data.List.drop 8 result)::Int
                    result = cppExposedHeuristic h s

--Increase a heuristic's score if s' has a greater OV than s
scoreHeuristic :: Heuristic -> Solution -> Solution -> Heuristic
scoreHeuristic h s s' = if snd s' > snd s then
                          (fst h, (snd h) + 1)
                        else
                          (fst h, (snd h) - 1)


-- ------------------------------EVOLVING HEURISTICS ---------------------------------------------------- --
--Create a new HeuristicPopulation by evolving the HeuristicRepresentations
evolveHeuristicPopulation :: HeuristicPopulation -> HeuristicPopulation
evolveHeuristicPopulation hs = do
                              let g = mkStdGen (snd(head hs))
                              let sortedhs = sortBy (comparing snd) hs
                              let (c1, c2) = evolveHeuristics (fst (sortedhs !! (length sortedhs -1)), fst (sortedhs !! (length sortedhs -2)))
                              let (shuffledNonParents, g2) = fisherYates g (tail (tail (reverse sortedhs)))
                              let mutatedHeuristic = mutateHeuristic g2 (fst (head shuffledNonParents))
                              let newHeuristicRepresentationSet = c1 : c2 : mutatedHeuristic : getAllHeuristicRepresentations (tail shuffledNonParents)
                              setScores newHeuristicRepresentationSet

--Choose two parents from the current population
selectParents :: HeuristicPopulation -> (HeuristicRepresentation, HeuristicRepresentation)
selectParents hs = (fst(ys !! (length ys -2)), fst(ys !! (length ys -1)))
                    where
                      ys = sortBy (comparing snd) hs
-- Naive solution - choose two best

--From two parents, create two new child heuristics
evolveHeuristics :: (HeuristicRepresentation, HeuristicRepresentation) -> (HeuristicRepresentation, HeuristicRepresentation)
evolveHeuristics (p1, p2) = (c1, c2)
                          where
                            c1 = Data.List.take i p1 ++ Data.List.drop i p2
                            c2 = Data.List.take i p2 ++ Data.List.drop i p1
                            i = getRandomIndex (mkStdGen 4) p1
--Performs One-Point Crossover at a "random" index

--Choose a heuristic from the current population to undergo mutation
selectMutateHeuristic :: StdGen -> HeuristicPopulation -> HeuristicRepresentation
selectMutateHeuristic g hs = reps !! x
                          where
                            x = getRandomIndex g reps
                            reps = getAllHeuristicRepresentations hs
--Selects a random HeuristicRepresentation to be mutated

-- --Mutate a heuristic
mutateHeuristic :: StdGen -> HeuristicRepresentation -> HeuristicRepresentation
mutateHeuristic g hr = Data.List.take x hr ++ [flipBit (hr !! x)] ++ Data.List.drop (x+1) hr
                    where
                      x = getRandomIndex g hr
--Performs one random bit flip

--Bit Flipper helper function to mutate heuristics
flipBit :: Char -> Char
flipBit '0' = '1'
flipBit '1' = '0'
flipBit _ = '0' --TODO do I need this?

--Helper function to attach initial scores to a set of HRs
setScores :: [HeuristicRepresentation] -> HeuristicPopulation
setScores [] = []
setScores hs = (head hs, 0) : setScores (tail hs)


-- ------------------------------CONTROL CODE ----------------------------------------------------------- --
main :: IO ()
main = do
  javacpp_init
  let heuristics = generateHeuristicPopulation
  print "Initial Heuristic Set:"
  print heuristics
  let solutions = generateSolutionPopulation
  print "Initial Solution Set:"
  print solutions
  putStrLn ""
  putStrLn ""
  print "Running:"
  let returnedData = runEvolutionaryCycles 10 (heuristics, solutions)
  let heuristics = fst returnedData
  let solutions = snd returnedData
  print "New Heuristic Set:"
  print heuristics
  print "New Solution Set:"
  print solutions
  putStrLn ""
  putStrLn ""
  print "Done!"

testAndEvaluate :: Int -> (HeuristicPopulation, SolutionPopulation) -> (HeuristicPopulation, SolutionPopulation)
testAndEvaluate 0 dat = dat
testAndEvaluate x dat = testAndEvaluate (x-1) (applyHeuristicPopulation dat)

runEvolutionaryCycles :: Int -> (HeuristicPopulation, SolutionPopulation) -> (HeuristicPopulation, SolutionPopulation)
runEvolutionaryCycles 0 dat = dat
runEvolutionaryCycles x (hs, sols) = runEvolutionaryCycles (x-1) (evolvedHs, newSols)
                                    where
                                      evolvedHs = evolveHeuristicPopulation scoredHs
                                      (scoredHs, newSols) = testAndEvaluate 5 (hs, sols)
