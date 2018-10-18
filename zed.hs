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


