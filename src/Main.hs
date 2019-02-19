module Main where

import Control.Concurrent
import Control.Parallel
import Data.Foldable
import System.Environment

readLines = fmap lines . readFile

odds [] = []
odds [x] = []
odds (x:y:xs) = y : odds xs

evns [] = []
evns [x] = [x]
evns (x:y:xs) = x : evns xs

merge [] [] = []
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x <= y = [x] ++ [y] ++ (merge xs ys)
    | x > y =  [y] ++ [x] ++ (merge xs ys)

batcherMerge [] = []
batcherMerge [x] = [x]
batcherMerge arr
    | length (arr) > 2 = [(head mergedEvn)] ++ mid ++ [(last mergedOdd)]
    | otherwise = merge [(head arr)] [(last arr)]
    where
        mergedEvn = batcherMerge (evns arr)
        mergedOdd = batcherMerge (odds arr)
        mid = merge (tail mergedEvn) (init mergedOdd)

batcherSort [] = []
batcherSort [x] = [x]
batcherSort arr = leftSorted `par` (rightSorted `pseq` (batcherMerge (leftSorted ++ rightSorted)))
    where
        (left,right) = splitList arr
        leftSorted = batcherSort left
        rightSorted = batcherSort right

splitList [] = ([],[])
splitList [x] = ([x],[])
splitList xs = splitAt mid xs where mid = (length xs) `div` 2

main = do
    args <- getArgs
    content <- readLines(head(args))
    print(batcherSort content)
