module CLIApp where

import Data.Maybe (fromJust)
import Numeric (readInt)
import System.Environment (getArgs)
import Text.Read (readMaybe)

readMaybeIntegral :: (Read i, Integral i) => String -> Maybe i
readMaybeIntegral = readMaybe

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "add" -> case numberOperator (tail args) (+) of
      Nothing -> putStrLn "Wrong format"
      Just result -> print $ show result
    _ -> print "No action supplied"

numberOperator :: (Read i, Integral i) => [String] -> (i -> i -> i) -> Maybe i
numberOperator [] _ = Nothing
numberOperator [_] _ = Nothing
numberOperator lst mapper =
  doAdd $ map readMaybeIntegral lst
  where
    doAdd maybeNumberList
      | Nothing `elem` maybeNumberList = Nothing
      | otherwise =
          let numbersToAdd = map fromJust maybeNumberList
           in Just $ foldl1 mapper numbersToAdd