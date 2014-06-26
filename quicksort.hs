{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (>= x) xs

prop_idempontent :: [Int] -> Bool
prop_idempontent xs =
  qsort (qsort xs) == qsort xs

prop_minimum :: [Int] -> Property
prop_minimum xs =
  not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered (qsort xs)
 where
  ordered [] = True
  ordered [x] = True
  ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation :: [Int] -> Bool
prop_permutation xs = permutation xs (qsort xs)
 where
  permutation xs ys =
    null (xs \\ ys) && null (ys \\ xs)

prop_sort_model :: [Int] -> Bool
prop_sort_model xs =
  sort xs == qsort xs

main = $quickCheckAll
