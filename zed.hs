import Data.List

-- Returns a list of all possible maps with side length n.
-- Essentially returns n combinations of n permutations.
getMaps :: Int -> [[[Int]]]
getMaps n = combinations (permutations [1..n]) []
  where
    combinations [] acc = if n == length acc then [acc] else []
    combinations (x:xs) acc
      | n == length acc = [acc]
      | otherwise = combinations xs (x:acc) ++ combinations xs acc

-- Helper function that simply checks if a list has all unique elements.
isUnique :: Eq a => [a] -> Bool
isUnique [] = True
isUnique (x:xs)
  | x `elem` xs = False
  | otherwise = isUnique xs

-- Checks the rows and columns for uniqueness for a given map.
hasUniqueRowsAndCols :: Eq a => [[a]] -> Bool
hasUniqueRowsAndCols kzMap = and (map isUnique kzMap) &&
                             and (map isUnique (transpose kzMap))

