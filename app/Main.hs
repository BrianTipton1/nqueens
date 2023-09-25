{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM, replicateM)
import Data.Bifunctor (bimap)
import Data.Data (Typeable)
import Data.Foldable (Foldable (foldl'), find)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (isSubsequenceOf, nub, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitWith)
import System.Process (system)
import System.Random (Random (randoms), newStdGen, randomRIO)
import Text.Read (readMaybe)

--- Helpers
redifyString :: String -> String
redifyString s = "\ESC[31m" ++ s ++ "\ESC[0m"

greenifyString :: String -> String
greenifyString str = "\x1b[32m" ++ str ++ "\x1b[0m"

split :: Char -> String -> [String]
split _ "" = [""]
split delimiter (x : xs)
  | x == delimiter = "" : rest
  | otherwise = (x : head rest) : tail rest
 where
  rest = split delimiter xs
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

randomSublist :: [a] -> IO [a]
randomSublist = filterM (\_ -> randomRIO (False, True))

randomElement :: [a] -> IO a
randomElement xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i

---

-- Cmd Line Stuff
data CmdInitBoard
  = CmdRandom
  | CmdDiagonal
  deriving (Eq)

instance Show CmdInitBoard where
  show :: CmdInitBoard -> String
  show = \case
    CmdRandom -> "Random"
    CmdDiagonal -> "Diagonal"

data CommandLineOption
  = CmdHelp
  | CmdInitBoard CmdInitBoard
  | CmdSize Int
  | CmdCrossover Double
  | CmdMutation Double
  | CmdPopulation Int
  | CmdGeneration Int
  deriving (Eq, Show)

cmdHelpList :: [String]
cmdHelpList = ["-h", "--help"]
cmdDiagonal :: [String]
cmdDiagonal = ["-d", "--diagonal"]
cmdRandom :: [String]
cmdRandom = ["-r", "--random"]
cmdSize :: String -> [String]
cmdSize s = (++ s) <$> ["-s=", "--size="]
cmdCrossover :: String -> [String]
cmdCrossover s = (++ s) <$> ["-c=", "--crossover="]
cmdMutation :: String -> [String]
cmdMutation s = (++ s) <$> ["-m=", "--mutation="]
cmdPopulation :: String -> [String]
cmdPopulation s = (++ s) <$> ["-p=", "--population="]
cmdGeneration :: String -> [String]
cmdGeneration s = (++ s) <$> ["-g=", "--generatin="]

strToOps :: String -> IO CommandLineOption
strToOps s
  | s `elem` cmdHelpList = return CmdHelp
  | s `elem` cmdRandom = return $ CmdInitBoard CmdRandom
  | s `elem` cmdDiagonal = return $ CmdInitBoard CmdDiagonal
  | s `elem` cmdSize size && isType @Int size = return $ CmdSize (read size :: Int)
  | s `elem` cmdCrossover size && isType @Double size = return $ CmdCrossover (read size :: Double)
  | s `elem` cmdMutation size && isType @Double size = return $ CmdMutation (read size :: Double)
  | s `elem` cmdPopulation size && isType @Int size = return $ CmdPopulation (read size :: Int)
  | s `elem` cmdGeneration size && isType @Int size = return $ CmdGeneration (read size :: Int)
  | otherwise = return CmdHelp
 where
  isType :: forall a. (Read a, Typeable a) => String -> Bool
  isType s = case readMaybe s :: Maybe a of
    Just _ -> True
    Nothing -> False
  size = last $ split '=' s

checkOps :: [CommandLineOption] -> IO [CommandLineOption]
checkOps ops
  | containsHelp ops
      || null ops
      || isSubsequenceOf [CmdInitBoard CmdRandom, CmdInitBoard CmdDiagonal] ops =
    do
      putStrLn helpScreen
      exitWith $ ExitFailure 1
  | otherwise = return ops
 where
  containsHelp = elem CmdHelp

helpScreen :: String
helpScreen =
  unlines
    [ "N-Queens Solver"
    , "====================="
    , ""
    , "Usage:"
    , "    nqueens [-s=SIZE] [-r | -d] [-c=PROB] [-m=PROB] [-p=SIZE] [-g=GEN]"
    , ""
    , "Description:"
    , "    This program solves the N-Queens problem, which involves placing N chess queens on an N×N chessboard so that no two queens threaten each other."
    , ""
    , "Options:"
    , "    -s=SIZE, --size=SIZE          Specifies the size of the board. SIZE must be an integer greater than 3. (Default: 8)"
    , "    "
    , "    -r, --random                  Solves the problem by placing queens at random positions that do not conflict with each other."
    , "    "
    , "    -d, --diagonal                Solves the problem by placing queens along the diagonal of the board before attempting to solve."
    , "    "
    , "    -c=PROB, --crossover=PROB     Specifies the crossover probability. PROB must be a decimal between 0 and 1."
    , "    "
    , "    -m=PROB, --mutation=PROB      Specifies the mutation probability. PROB must be a decimal between 0 and 1."
    , "    "
    , "    -p=SIZE, --population=SIZE    Specifies the population size. SIZE must be an integer."
    , "    "
    , "    -g=GEN, --generation=GEN      Specifies the number of generations. GEN must be an integer."
    , "  "
    , "Note:"
    , "    Either the -r/--random flag or the -d/--diagonal flag can be supplied; both cannot be used at the same time."
    , ""
    , "Examples:"
    , "    nqueens -r             "
    , "    nqueens -s=8 -r"
    , "    nqueens --size=10 --diagonal"
    , "    nqueens -s=10 -r -c=0.7 -m=0.2 -p=50 -g=100"
    ]

getDefaultOrOption :: [CommandLineOption] -> CommandLineOption -> CommandLineOption
getDefaultOrOption opts defaultOpt =
  fromMaybe defaultOpt (find (matchesType defaultOpt) opts)
 where
  matchesType :: CommandLineOption -> CommandLineOption -> Bool
  matchesType CmdHelp CmdHelp = True
  matchesType (CmdInitBoard CmdDiagonal) (CmdInitBoard CmdDiagonal) = True
  matchesType (CmdInitBoard CmdRandom) (CmdInitBoard CmdRandom) = True
  matchesType (CmdSize _) (CmdSize _) = True
  matchesType (CmdCrossover _) (CmdCrossover _) = True
  matchesType (CmdMutation _) (CmdMutation _) = True
  matchesType (CmdPopulation _) (CmdPopulation _) = True
  matchesType (CmdGeneration _) (CmdGeneration _) = True
  matchesType _ _ = False

processInitBoard :: [CommandLineOption] -> Int -> Int -> IO (CmdInitBoard, Population)
processInitBoard opts size popsize = do
  let initBoardOpt = filter isCmdInitBoard opts
  case initBoardOpt of
    (CmdInitBoard b : _) -> do
      population <- handleInitBoard b
      return (b, population)
    _ -> do
      population <- handleInitBoard CmdDiagonal
      return (CmdDiagonal, population)
 where
  isCmdInitBoard (CmdInitBoard _) = True
  isCmdInitBoard _ = False
  handleInitBoard CmdRandom = replicateM popsize $ randomNSizeBoard size
  handleInitBoard CmdDiagonal = return $ replicate popsize $ createDiagonalBoard size

-- End Cmd Line stuff

--- Types Relating to the Assignment

-- Defs
newtype Queen where
  Queen :: {queen :: Int} -> Queen
  deriving (Show)

newtype Rank where
  Rank :: {rank :: Int} -> Rank
  deriving (Show, Eq)

newtype File where
  File :: {file :: Int} -> File
  deriving (Show, Eq)

newtype Coordinate where
  Coordinate :: {coordinate :: (Rank, File)} -> Coordinate
  deriving (Show, Eq)

newtype LookupRow where
  LookupRow :: {lookupRow :: (Queen, Coordinate)} -> LookupRow

type Row = Int
type Column = Int

type BoardSize = Int

type Dims = (Row, Column)

type MutationRate = Double
type CrossoverRate = Double

type Population = [LookupTable]

data LookupTable where
  LookupTable ::
    { lookupTable :: ![LookupRow]
    , deminsions :: !Dims
    } ->
    LookupTable

data Configuration where
  Configuration ::
    { boardSize :: !Int
    , popSize :: !Int
    , maxGen :: !Int
    , generationSize :: !Int
    , mutationRate :: !Double
    , crossOverRate :: !Double
    , initBoard :: !CmdInitBoard
    } ->
    Configuration

-- End Defs

--Instances
instance Show LookupTable where
  show :: LookupTable -> String
  show (LookupTable lookupTbl (row, col)) =
    unlines (map (concatMap prettyPrint) rows)
   where
    rows = [[(r, c) | c <- [1 .. col]] | r <- [1 .. row]]
    lookupMap = [(coordinateValue coord, queen q) | LookupRow (q, coord) <- lookupTbl]
    prettyPrint (r, c) = case lookup (r, c) lookupMap of
      Just n -> colorize (r, c) $ "| " ++ show n ++ " |"
      Nothing -> colorize (r, c) "| _ |"

    colorize (r, c) str
      | even (r + c) = redifyString str
      | otherwise = greenifyString str

instance Show LookupRow where
  show :: LookupRow -> String
  show (LookupRow (Queen q, Coordinate (Rank r, File c))) =
    "Queen " ++ show q ++ " at (" ++ show r ++ ", " ++ show c ++ ")"

instance Show Configuration where
  show :: Configuration -> String
  show
    Configuration
      { boardSize = brdSize
      , popSize = popSize
      , generationSize = currGen
      , maxGen = maxGen
      , mutationRate = mutRate
      , crossOverRate = cRate
      , initBoard = initBoard
      } =
      "Initial Board Layout: " ++ show initBoard
        ++ "\nBoard Size: "
        ++ show brdSize
        ++ " | "
        ++ "Population Size: "
        ++ show popSize
        ++ " | "
        ++ "Generation Number: "
        ++ show currGen
        ++ " | "
        ++ "Max Generations: "
        ++ show maxGen
        ++ " | "
        ++ "Mutation Rate: "
        ++ show (100 * mutRate)
        ++ "% | "
        ++ "Crossover Rate: "
        ++ show (100 * cRate)
        ++ "%"

--Instances

--- End types Relating to the Assignment

initializeLookupTable :: Dims -> [Coordinate] -> LookupTable
initializeLookupTable dims coords =
  LookupTable (zipWith (curry LookupRow) queens coords) dims
 where
  queens = map Queen [1 .. length coords]

-- Setting up the board
coordinateValue :: Coordinate -> (Int, Int)
coordinateValue = bimap rank file . coordinate
rowValue :: Coordinate -> Int
rowValue = rank . fst . coordinate
columnValue :: Coordinate -> Int
columnValue = file . snd . coordinate

randomNPositions :: Int -> IO [Coordinate]
randomNPositions n = do
  let allPossibleTuples = [(Rank i, File j) | i <- [1 .. n], j <- [1 .. n]]
  shuffledTuples <- shuffle allPossibleTuples
  return $ map Coordinate $ take n (nub shuffledTuples)
 where
  shuffle :: [a] -> IO [a]
  shuffle xs = do
    gen <- newStdGen
    return $ map fst . sortBy (compare `on` snd) $ zip xs (randoms gen :: [Int])

diagonalCoordinates :: BoardSize -> [Coordinate]
diagonalCoordinates n = zipWith makeCoord [1 .. n] [1 .. n]
 where
  makeCoord x y = Coordinate (Rank x, File y)

createDiagonalBoard :: BoardSize -> LookupTable
createDiagonalBoard n = LookupTable diagonalLookup (n, n)
 where
  diagonalCoords = diagonalCoordinates n
  diagonalLookup = zipWith makeLookupRow [1 .. n] diagonalCoords

  makeLookupRow q coord = LookupRow (Queen q, coord)

clsPrint :: LookupTable -> Configuration -> IO ()
clsPrint b@(LookupTable lookupRow _) cfg = do
  system "clear"
  print b
  print cfg
  putStrLn $ "Num Threats: " ++ show (numThreats b)
  putStr $ unlines $ map show lookupRow

solved8QueensBoard :: LookupTable
solved8QueensBoard = initializeLookupTable (8, 8) coordinates
 where
  coordinates =
    [ Coordinate (Rank 1, File 6)
    , Coordinate (Rank 2, File 4)
    , Coordinate (Rank 3, File 7)
    , Coordinate (Rank 4, File 1)
    , Coordinate (Rank 5, File 8)
    , Coordinate (Rank 6, File 2)
    , Coordinate (Rank 7, File 5)
    , Coordinate (Rank 8, File 3)
    ]

randomNSizeBoard :: Int -> IO LookupTable
randomNSizeBoard n =
  randomNPositions n <&> initializeLookupTable (n, n)

--- Algorithm Stuff

queensThreaten :: Coordinate -> Coordinate -> Bool
queensThreaten (Coordinate (Rank r1, File c1)) (Coordinate (Rank r2, File c2)) =
  r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)

numThreats :: LookupTable -> Int
numThreats (LookupTable lookupTbl dims) = countThreats coords
 where
  coords = map (snd . lookupRow) lookupTbl
  countThreats [] = 0
  countThreats (x : xs) = length (filter (queensThreaten x) xs) + countThreats xs

evolvePopulation :: MutationRate -> CrossoverRate -> Population -> Configuration -> IO Population
evolvePopulation mutationRate crossoverRate initialPopulation cfg = do
  let eliteSize = min 4 (length initialPopulation `div` 4)
      sortedPop = sortOn numThreats initialPopulation
      elites = take eliteSize sortedPop
      totalFitness = sum (map numThreats sortedPop)

  parent1 <- randSelection sortedPop totalFitness

  clsPrint (head elites) cfg

  randomBoard <- randomNSizeBoard (boardSize cfg)

  let newElites = parent1 : randomBoard : elites

  children <- replicateM (popSize - length newElites) (performCrossover sortedPop)

  return $ newElites ++ children
 where
  popSize = length initialPopulation
  randSelection :: Population -> Int -> IO LookupTable
  randSelection population totalFitness = do
    randomFitness <- randomRIO (0, totalFitness)
    let selected = findParent population randomFitness 0
    return selected

  findParent :: Population -> Int -> Int -> LookupTable
  findParent [] _ _ = error "Something bad has occurred :("
  findParent (p : ps) randomFitness acc
    | acc + numThreats p >= randomFitness = p
    | otherwise = findParent ps randomFitness (acc + numThreats p)

  performCrossover :: Population -> IO LookupTable
  performCrossover population = do
    parent1 <- randomElement population
    parent2 <- randomElement population
    child <- crossover crossoverRate parent1 parent2
    mutate mutationRate cfg child

crossover :: CrossoverRate -> LookupTable -> LookupTable -> IO LookupTable
crossover rate parent1@(LookupTable _ dims1) parent2@(LookupTable _ _) = do
  r <- randomRIO (0.0, 1.0)
  if r < rate
    then do
      crossoverPoint <- randomRIO (1, length (lookupTable parent1) - 1)
      let (firstHalf, _) = splitAt crossoverPoint (lookupTable parent1)
          (_, secondHalf) = splitAt crossoverPoint (lookupTable parent2)

      let firstHalfPositions = map ((coordinate . snd) . lookupRow) firstHalf
      let secondHalfPositions = map ((coordinate . snd) . lookupRow) secondHalf
      let collisionExists = any (`elem` firstHalfPositions) secondHalfPositions

      if collisionExists
        then crossover rate parent1 parent2
        else return $ LookupTable (firstHalf ++ secondHalf) dims1
    else return parent1

mutate :: MutationRate -> Configuration -> LookupTable -> IO LookupTable
mutate rate cfg child = do
  let LookupTable tableRows dims = child
  mutated <- forM tableRows $ \row -> do
    r <- randomRIO (0.0, 1.0)
    if r < rate
      then do
        let (n, _) = dims
        let occupiedPositions = map (coordinate . snd . lookupRow) tableRows
        let unoccupiedPositions =
              [(Rank r, File c) | r <- [1 .. n], c <- [1 .. n], (Rank r, File c) `notElem` occupiedPositions]
        newPos <- randomElement unoccupiedPositions
        return $ updateRow (queen . fst $ lookupRow row) newPos row
      else return row
  return $ LookupTable mutated dims
 where
  updateRow i newPos row@(LookupRow (q, _))
    | queen q == i = LookupRow (q, Coordinate newPos)
    | otherwise = row

main :: IO ()
main = do
  args <- getArgs
  ops <- mapM strToOps args >>= checkOps
  let (CmdCrossover crssover) = getDefaultOrOption ops (CmdCrossover 0.7)
      (CmdMutation mut) = getDefaultOrOption ops (CmdMutation 0.01)
      (CmdPopulation pop) = getDefaultOrOption ops (CmdPopulation 32)
      (CmdGeneration gen) = getDefaultOrOption ops (CmdGeneration maxBound)
      (CmdSize size) = getDefaultOrOption ops (CmdSize 8)

  (cmdInit, initialPopulation) <- processInitBoard ops size pop
  let config =
        Configuration
          { popSize = pop
          , mutationRate = mut
          , maxGen = gen
          , generationSize = 0
          , crossOverRate = crssover
          , boardSize = size
          , initBoard = cmdInit
          }

  let loop acc population = do
        newPopulation <- evolvePopulation mut crssover population (config{generationSize = acc})
        let maybeSol = find ((== 0) . numThreats) newPopulation
            bestFit = head $ sortOn numThreats newPopulation
        if acc < gen
          then case maybeSol of
            Just solution -> do
              putStrLn "Found a solution:"
              putStrLn $ "Num generations taken: " ++ show acc ++ " generations"
              print solution
              exitSuccess
            Nothing -> loop (acc + 1) newPopulation
          else do
            putStrLn ("\nFailed to find solution in " ++ show gen ++ " generations")
            putStrLn "Best Board: "
            print bestFit
            putStrLn $ "Num Threats: " ++ show (numThreats bestFit)
   in loop 0 initialPopulation
