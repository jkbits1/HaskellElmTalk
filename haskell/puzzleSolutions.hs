import Data.List
import Numeric

-- for profiling
import System.Environment
import Text.Printf

getCounterBase = 8
--lazy2startPos = 0

-- test data that is easier to visualise and debug
first =     [1,2,3]
second =    [4,5,6]
three =     [7,8,9]

answers = [12, 15, 18]

-- first =     [6, 5, 5, 6, 5, 4, 5, 4]
-- second =    [4, 2, 2, 2, 4, 3, 3, 1]
-- three =     [1, 3, 2, 3, 3, 2, 4, 3]

-- answers = [12, 8, 12, 10, 10, 12, 10, 8]

type WheelPosition    = [Int]
type WheelLoop        = [WheelPosition]
type LoopsPermutation = [WheelPosition]
type LoopsPermColumn  = (Int, Int, Int)
type LoopsPermAnswers = [Int]
type Counter          = [Int]

turnWheel :: WheelPosition -> Int -> WheelPosition
turnWheel wheel chunk = (drop chunk wheel) ++ (take chunk wheel)

buildWheelLoop :: [WheelPosition] -> WheelPosition -> Int -> WheelLoop
buildWheelLoop positions pos 0 = positions ++ [pos]
buildWheelLoop positions pos count =
  let
    nextPositions = (positions ++ [turnWheel pos count])
  in
    buildWheelLoop nextPositions pos (count-1)

wheelLoopFromStartPos :: WheelPosition -> WheelLoop
wheelLoopFromStartPos pos = buildWheelLoop [] pos $ (length pos) - 1

secLoop :: WheelLoop
secLoop = wheelLoopFromStartPos second
-- [ [4,5,6], [5,6,4] ... ]

thrLoop :: WheelLoop
thrLoop = wheelLoopFromStartPos three
-- [ [7,8,9], [8,9,7] ... ]

-- NOTE - used for revised first solution and lazy eval solution
ansLoop :: WheelLoop
ansLoop = wheelLoopFromStartPos answers

twoWheelPerms :: [LoopsPermutation]
twoWheelPerms = map (\secPos -> first : secPos : []) secLoop
-- result - [ [[1,2,3], [4,5,6]],
--            [[1,2,3], [5,6,4]], ...]

-- could refactor as a let clause, but then lose types
appendTwoWheelPerms :: WheelPosition -> [LoopsPermutation]
appendTwoWheelPerms thrPos =
  map (\twoLoopPerm ->  twoLoopPerm ++ [thrPos]) twoWheelPerms
-- param = [7,8,9]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]

threeWheelPerms :: [LoopsPermutation]
threeWheelPerms = concat $ map appendTwoWheelPerms thrLoop
-- result -
-- [ [[1,2,3],[4,5,6],[7,8,9]],  [[1,2,3],[5,4,6],[7,8,9]] ...,
--   [[1,2,3],[4,5,6],[8,7,9]],  [[1,2,3],[5,4,6],[8,7,9]] ... ]

sumTriple :: LoopsPermColumn -> Int
sumTriple (a, b, c) = a + b + c

columnsFromPerm :: LoopsPermutation -> [LoopsPermColumn]
columnsFromPerm perm =
    let firstPos  = head perm
        secPos    = head $ drop 1 perm
        thrPos    = head $ drop 2 perm
    in
        zip3 firstPos secPos thrPos

sumPlusPerm :: LoopsPermutation -> [(LoopsPermAnswers, LoopsPermutation)]
sumPlusPerm perm = [(map sumTriple $ columnsFromPerm perm, perm)]

answersPlusList :: [(LoopsPermAnswers, LoopsPermutation)]
answersPlusList = concat $ map sumPlusPerm threeWheelPerms

findSpecificAnswer :: (LoopsPermAnswers, LoopsPermutation)
findSpecificAnswer =
  head $
    dropWhile (\(answer, _) -> (elem answer ansLoop) == False) answersPlusList


-- 3rd solution, merges perms

twoWheelPermsIndexed :: [a] -> [(Int, a)]
twoWheelPermsIndexed perms = zip [1..] perms

twoWheelPermsAdded :: [[Int]] -> [Int]
twoWheelPermsAdded items = zipWith (+) (head items) (head $ drop 1 items)

mergeTwoWheelPerms :: [[Int]]
mergeTwoWheelPerms = map twoWheelPermsAdded twoWheelPerms

indexedResults :: [(Int, [Int])]
indexedResults = twoWheelPermsIndexed mergeTwoWheelPerms

appendTwoWheelPermsX :: [Int] -> [(Int, [Int])]
appendTwoWheelPermsX thrPos =
  map (\(i, twoLoopResults) ->  (i, zipWith (+) twoLoopResults thrPos)) indexedResults
-- param = [7,8,9]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]

threeWheelPermsX :: [(Int, [Int])]
threeWheelPermsX = concat $ map appendTwoWheelPermsX thrLoop

findSpecificAnswerX =
  let
    (answerIndex, correctAnswer) =
      head $ filter (\(idx, answer) -> elem answer ansLoop) threeWheelPermsX
  in
    threeWheelsPermsItemByCounter $ getCounter answerIndex


-- 2nd solution, works in constant space

-- | Returns the digits of a positive integer as a list, in reverse order.
--   This is slightly more efficient than in forward order.
digitsRev :: Integral n
    => n -- ^ The base to use.
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form, in reverse.
digitsRev base i = case i of
        0 -> []
        _ -> lastDigit : digitsRev base rest
    where (rest, lastDigit) = quotRem i base

-- | Returns the digits of a positive integer as a list.
digits :: Integral n
    => n -- ^ The base to use (typically 10).
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form.
digits base = reverse . digitsRev base

--counter/state (each digit up to 7)
initCounter :: Counter
initCounter = [0,0,0]


getCounter :: Int -> (Int, Counter)
getCounter x =
  case x < 0 of
    True -> getCounter 0
    False ->
      let
        xs = digits getCounterBase x
      in
        case length xs of
          0 -> (x, [0,0])
          1 -> (x, [0] ++ xs)
          otherwise -> (x, xs)

wheelPermsItem :: Int -> WheelLoop -> WheelPosition
wheelPermsItem idx loop = head $ drop idx loop

threeWheelsPermsItemByCounter :: (a, Counter) -> LoopsPermutation
threeWheelsPermsItemByCounter (_, counter) =
  let
    sec_idx = head counter
    thr_idx = head $ drop 1 counter
   in
    [first]
    ++
      [wheelPermsItem sec_idx secLoop] ++
      [wheelPermsItem thr_idx thrLoop]

wheelsTuple :: LoopsPermutation -> [LoopsPermColumn]
wheelsTuple xxs =
  let
    inn = head xxs
    sec = head $ drop 1 xxs
    thr = head $ drop 2 xxs
  in
    zip3 inn sec thr

getWheelsPermAnswers :: Int -> LoopsPermAnswers
getWheelsPermAnswers n =
  mapTest sumTriple $ wheelsTuple $ threeWheelsPermsItemByCounter $ getCounter n

mapTest f xs = foldr (\x ys -> f x : ys) [] xs


-- create a list that has ruled out failures by a quick check of one value
findAnswerTest =
  [ i |
        i <- [0..(getCounterBase*getCounterBase)],
        let ans = elem (head (getWheelsPermAnswers i)) answers,
        ans == True ]

-- gets first true result, but otherwise has no stopping condition
findAnswerCS1 =
  take 1 [ i |
              i <- [0..],
              let ans = elem (getWheelsPermAnswers i) ansLoop,
              ans == True ]

findAnswerCS2a  = findAnswerCS2base [0..]
findAnswerCS2b  = findAnswerCS2base findAnswerTest

-- list comprehension rewritten
findAnswerCS2base xs =
  let
    ansH =
      fst $
      head $
        dropWhile (\(_, b) -> b == False)
          $ map (\i -> (i, elem (getWheelsPermAnswers i) ansLoop))
            xs
   in
    threeWheelsPermsItemByCounter $ getCounter ansH


--
-- assorted load test values
--

--first =     [1,  2,  3,  4]
--first =     [2,  3,  4,  1]
--second =    [5,  6,  7,  8]
--three =     [9,  10, 11, 12]
--
--answers =   [15, 18, 21, 24]
--getCounterBase = 24

--first =     [ 1,  2,  3,  4,  5]
--first =     [ 2,  3,  4,  5,  1]
--second =    [ 6,  7,  8,  9, 10]
--three =     [11, 12, 13, 14, 15]
--
--answers =   [18, 21, 24, 27, 30]
--getCounterBase = 120

--first =     [ 1,  2,  3,  4,  5,  6]
--first =     [ 2,  3,  4,  5,  6,  1]
--second =    [ 7,  8,  9, 10, 11, 12]
--three =     [13, 14, 15, 16, 17, 18]
--answers =   [21, 24, 27, 30, 33, 36]
--getCounterBase = 720

--first =     [1,   2,  3,  4,  5,  6,  7]
--first =     [7,   1,  2,  3,  4,  5,  6]
--second =    [8,   9, 10, 11, 12, 13, 14]
--three =     [15, 16, 17, 18, 19, 20, 21]
--
--answers =   [24, 27, 30, 33, 36, 39, 42] -- for 1..7
--getCounterBase = 5040

main :: IO ()
main = methodChoice

methodChoice :: IO ()
methodChoice =
  let
    results choice =
      case choice of
        "0" ->        show $ findAnswerCS1
        "1" ->        show $ head findAnswerCS2a
        "2" ->        show $ head findAnswerCS2b
        "3" ->        show $ findSpecificAnswer
        "4" ->        show $ findSpecificAnswerX
  in
    do
      putStrLn
        ("0 - findAnswerCS1, 1 - findAnswerCS2a, 2 - findAnswerCS2b, " ++
        ", 3 - findSpecificAnswer, 4 - findSpecificAnswerX")
      input1 <- getLine :: IO String
      putStrLn input1
      putStrLn $ results input1


-- use these settings for load testing
--wheelLoopFromStartPos pos = permutations pos


-- use ghci to build up code

-- findSpecificAnswer
-- ([12,15,18],[[1,2,3],[4,5,6],[7,8,9]])

-- fst findSpecificAnswer
-- [12,15,18]

-- length $ fst findSpecificAnswer

-- head $ snd findSpecificAnswer

-- turnWheel (fst findSpecificAnswer) 1
-- [15,18,12]

-- turnWheel (head $ snd findSpecificAnswer) 1

-- ansTurn n = (turnWheel (fst findSpecificAnswer) n, 
--             turnWheel (head $ snd findSpecificAnswer) n)

ansTurn :: Int -> (WheelPosition, [WheelPosition])
ansTurn n = (turnWheel (fst findSpecificAnswer) n, 
              map (flip turnWheel $ n) (snd findSpecificAnswer) )

findScreenAns :: WheelPosition -> Int -> ([Int], [WheelPosition])
findScreenAns _   0 = ([], [])
findScreenAns ans n =
   let ansT = fst $ ansTurn n
   in
     case ansT == ans of
       True   -> ansTurn n
       False  -> findScreenAns ans $ n - 1

findScreenAnsX :: WheelPosition -> ([Int], [WheelPosition])
findScreenAnsX ans =
  findScreenAns ans $ length $ fst findSpecificAnswer

