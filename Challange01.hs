module Challange01 where

import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Challange 01: Task 1

-- 1) Searching
indexOf :: Eq a => a -> [a] -> Int
indexOf _ [] = -1
indexOf el lst =
  let withI idx el (head : tail)
        | head == el = idx
        | null tail = -1
        | otherwise = withI (idx + 1) el tail
   in withI 0 el lst

_isLower :: Char -> Bool
_isLower char = char `elem` ['a' .. 'z']

charToLower :: Char -> Char
charToLower char
  | _isLower char = char
  | otherwise = ['a' .. 'z'] !! idx
  where
    idx = indexOf char ['A' .. 'Z']

strToLower :: String -> String
strToLower = map charToLower

-- 1a)
findPos :: String -> [String] -> Int
findPos _ [] = -1
findPos str lst =
  indexOf (strToLower str) (map strToLower lst)

-- 1b)
findCharString :: Char -> [String] -> [String]
findCharString _ [] = []
findCharString _char lst =
  let char = charToLower _char
   in filter (\e -> char `elem` strToLower e) lst

-- 2) Sorting

insertSorted :: Ord a => a -> [a] -> [a]
insertSorted elem [] = [elem]
insertSorted elem (head : tail)
  | elem < head = elem : (head : tail)
  | elem > head = head : insertSorted elem tail
  | otherwise = head : tail

sortStrings :: [String] -> [String]
sortStrings [] = []
sortStrings inputList =
  let lowerCaseList = map strToLower inputList
   in map (\e -> inputList !! indexOf e lowerCaseList) (doInsert [] lowerCaseList)
  where
    doInsert dest (head : rest)
      | null rest = insertSorted head dest
      | otherwise = doInsert (insertSorted head dest) rest

-- 3) Name Scores

getCharScore :: Char -> Int
getCharScore char =
  let score = indexOf (charToLower char) ['a' .. 'z']
   in if score == -1 then 0 else score

nameScore :: String -> [String] -> Integer
nameScore _ [] = 0
nameScore _ [_] = 0
nameScore name sortedList
  | name `notElem` sortedList = -1
  | otherwise = ownScore * toInteger (indexOf name sortedList)
  where
    ownScore =
      sum $ map (\it -> toInteger $ getCharScore it + 1) name

-- nameScore "Marek" $ sortStrings ["Ivan", "Adam", "Petr", "Bill"] -> -1
-- nameScore "Bill" $ sortStrings ["Ivan", "Adam", "Petr", "Bill"] -> 35

-- Task 2
-- 1)

readMaybeIntegral :: (Read i, Integral i) => String -> Maybe i
readMaybeIntegral = readMaybe

readConsole :: IO ()
readConsole = do
  putStr "Give a Number: "
  numberRead <- getLine
  let digitSum = doDigitSum $ stringToDigitMap numberRead
   in case digitSum of
        Nothing -> putStrLn "Enter a valid number"
        Just num -> putStrLn $ printf "The sum is %d" num
  where
    stringToDigitMap :: String -> [Maybe Int]
    stringToDigitMap = map $ readMaybeIntegral . (: [])

    doDigitSum :: (Read i, Integral i) => [Maybe i] -> Maybe i
    doDigitSum digitMap
      | Nothing `elem` digitMap = Nothing
      | otherwise = Just (sum $ map (fromMaybe $ -1) digitMap)

-- 2)