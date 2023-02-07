module Challange01 where

-- Challange 01: Task 1

-- 1) Searching
_indexOf :: Eq a => a -> [a] -> Int
_indexOf _ [] = -1
_indexOf el lst =
  let withI idx el (head : tail)
        | head == el = idx
        | null tail = -1
        | otherwise = withI (idx + 1) el tail
   in withI 0 el lst

_isLower :: Char -> Bool
_isLower char = char `elem` ['a' .. 'z']

_charToLower :: Char -> Char
_charToLower char
  | _isLower char = char
  | otherwise = ['a' .. 'z'] !! idx
  where
    idx = _indexOf char ['A' .. 'Z']

_toLower :: String -> String
_toLower = map _charToLower

-- 1a)
findPos :: String -> [String] -> Int
findPos _ [] = -1
findPos str lst =
  _indexOf (_toLower str) (map _toLower lst)

-- 1b)
findCharString :: Char -> [String] -> [String]
findCharString _ [] = []
findCharString _char lst =
  let char = _charToLower _char
   in filter (\e -> char `elem` _toLower e) lst

-- 2) Sorting

_insertSorted :: Ord a => a -> [a] -> [a]
_insertSorted elem [] = [elem]
_insertSorted elem (head : tail)
  | elem < head = elem : (head : tail)
  | elem > head = head : _insertSorted elem tail
  | otherwise = head : tail

sortStrings :: [String] -> [String]
sortStrings [] = []
sortStrings inputList =
  let lowerCaseList = map _toLower inputList
   in map (\e -> inputList !! _indexOf e lowerCaseList) (doInsert [] lowerCaseList)
  where
    doInsert dest (head : rest)
      | null rest = _insertSorted head dest
      | otherwise = doInsert (_insertSorted head dest) rest

-- 3) Name Scores

_getCharScore :: Char -> Int
_getCharScore char =
  let score = _indexOf (_charToLower char) ['a' .. 'z']
   in if score == -1 then 0 else score

nameScore :: String -> [String] -> Integer
nameScore _ [] = 0
nameScore _ [_] = 0
nameScore name sortedList
  | name `notElem` sortedList = -1
  | otherwise = ownScore * toInteger (_indexOf name sortedList)
  where
    ownScore =
      sum $ map (\it -> toInteger $ _getCharScore it + 1) name

-- nameScore "Marek" $ sortStrings ["Ivan", "Adam", "Petr", "Bill"] -> -1
-- nameScore "Bill" $ sortStrings ["Ivan", "Adam", "Petr", "Bill"] -> 35
